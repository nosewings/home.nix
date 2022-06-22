{ config, lib, pkgs, ... }:
with lib;
with types;
let
  cfg = config.programs.emacs.init;

  removeKey = key: filterAttrs (k: _: k != key);

  boolToElisp = bool: if bool then "t" else "nil";
  stringToElisp = string: "\"${string}\"";

  packageType = submodule ({ name, ... }: {
    options = {
      enable = mkEnableOption "Emacs package ${name}.";
      package = mkOption {
        type = nullOr (functionTo package);
        default = epkgs: epkgs.${name};
      };
      loadWhenBuilding = mkOption {
        type = bool;
        default = false;
      };
      after = mkOption {
        type = listOf str;
        default = [];
      };
      bind = mkOption {
        type = attrsOf (attrsOf str);
        default = {};
      };
      bind-keymap = mkOption {
        type = attrsOf (attrsOf str);
        default = {};
      };
      commands = mkOption {
        type = listOf str;
        default = [];
      };
      init = mkOption {
        type = lines;
        default = "";
      };
      config = mkOption {
        type = lines;
        default = "";
      };
      custom = mkOption {
        type = attrsOf str;
        default = {};
      };
      defer = mkOption {
        type = oneOf [ bool int ];
        default = false;
      };
      ensure = mkOption {
        type = bool;
        default = false;
      };
      hook = mkOption {
        type = attrsOf (listOf str);
        default = {};
      };
      mode = mkOption {
        type = attrsOf (nullOr str);
        default = {};
      };
      preface = mkOption {
        type = lines;
        default = "";
      };
      no-require = mkOption {
        type = bool;
        default = true;
      };
    };
  });

  mkBindGeneral = keyword: bind: optionalString (bind != {}) (
    let
      globalBinds = bind."" or {};
      nonGlobalBinds = removeKey "" bind;
      mkBindings = bindings: concatStringsSep "\n" (mapAttrsToList (key: bind: "(${key} . ${bind})") bindings);
      mkNonGlobalBindings = keymap: bindings: ":map ${keymap}\n" + mkBindings bindings;
    in
      ":${keyword} (${concatStringsSep "\n" (filter (x: x != "") [
        (mkBindings globalBinds)
        (concatStringsSep "\n" (mapAttrsToList mkNonGlobalBindings nonGlobalBinds))
      ])})"
  );

  mkAfter = after: optionalString (after != []) ''
    :after ${concatStringsSep " " after}'';

  mkBindKeymap = mkBindGeneral "bind-keymap";

  mkBind = mkBindGeneral "bind";

  mkCommands = commands: optionalString (commands != []) (
    if isString commands then
      ":commands ${commands}"
    else
      ":commands ${concatStringsSep "\n" commands}"
  );

  mkInit = init: optionalString (init != "") ''
    :init
    ${init}'';

  mkConfig = config: optionalString (config != "") ''
    :config
    ${config}'';

  mkCustom = custom: optionalString (custom != {}) ''
    :custom
    ${concatStringsSep "\n" (mapAttrsToList (key: value: "(${key} ${value})") custom)}'';

  mkDefer = defer: optionalString (isInt defer || defer)
    ":defer ${if isBool defer then boolToElisp defer else toString defer}";

  mkEnsure = ensure: optionalString ensure
    ":ensure ${boolToElisp ensure}";

  mkHook = hook: optionalString (hook != {})
    ":hook (${concatStringsSep "\n" (flatten (mapAttrsToList (hook:
      map (function: if hook == "" then function else "(${hook} . ${function})")) hook))})";

  mkMode = mode: optionalString (mode != {})
    ":mode (${concatStringsSep "\n" (mapAttrsToList (pattern: mode:
      if mode == null then stringToElisp pattern else "(${stringToElisp pattern} . ${mode})"
    ) mode)})";

  mkPreface = preface: optionalString (preface != "") ''
    :preface
    ${preface}'';

  mkNoRequire = noRequire: optionalString noRequire
    ":no-require ${boolToElisp noRequire}";

  mkPackageString = name: package:
    concatStringsSep "\n" (["(use-package ${name}"] ++ filter (x: x != "") [
      (mkAfter package.after)
      (mkBindKeymap package.bind-keymap)
      (mkBind package.bind)
      (mkCommands package.commands)
      (mkInit package.init)
      (mkConfig package.config)
      (mkCustom package.custom)
      (mkDefer package.defer)
      (mkEnsure package.ensure)
      (mkHook package.hook)
      (mkMode package.mode)
      (mkPreface package.preface)
      (mkNoRequire package.no-require)
    ]) + ")";
  mkInitOption = {
    prelude = mkOption {
      type = lines;
      default = "";
    };
    packages = mkOption {
      type = attrsOf packageType;
      default = { };
    };
  };
in
{
  options.programs.emacs.init = {
    enable = mkEnableOption "Emacs init files";
    earlyInit = mkInitOption;
    init = mkInitOption;
  };

  config = mkIf cfg.enable {
    programs.emacs.enable = true;
    programs.emacs.extraPackages = epkgs:
      let
        # Create an Emacs init derivation; i.e., a derivation that
        # contains an Emacs init file, either init.el or
        # early-init.el.
        mkInitDrv = name: oldPkgs: newPkgs: prelude:
          let
            # A function that computes whether a given package set
            # requires use-package.
            needsUsePackage = pkgs: pkgs != { };
            # Whether the current derivation needs to require
            # use-package.  This occurs if the previous package set
            # did not require it AND the current set does.
            requireUsePackage = !needsUsePackage oldPkgs && needsUsePackage newPkgs;
            # Whether the current derivation needs to depend on
            # use-package.
            dependUsePackage = needsUsePackage oldPkgs || needsUsePackage newPkgs;
            # A function that computes whether a given package set
            # requires bind-key.
            needsBindKey = pkgs: any (pkg: pkg.bind != { } || pkg.bind-keymap != { }) (attrValues pkgs);
            # Whether the current derivation needs to require
            # bind-key.  As with use-package, this occurs if none of
            # the old packages needed it AND any of the new packages
            # needs it.
            requireBindKey = !needsBindKey oldPkgs && needsBindKey newPkgs;
            # Whether the current derivation needs to depend on
            # bind-key.
            dependBindKey = needsBindKey oldPkgs || needsBindKey newPkgs;
            # A list of all Nix derivations that this derivation
            # depends on.
            dependencies = optional dependUsePackage epkgs.use-package ++ optional dependBindKey epkgs.bind-key ++ concatLists (mapAttrsToList (_: pkg: optional (pkg.package != null) (pkg.package epkgs)) (oldPkgs // newPkgs));
          in
            epkgs.trivialBuild rec {
              pname = "emacs-${name}";
              packageRequires = dependencies;
              buildInputs = dependencies;
              src = pkgs.writeText "${pname}.el" (concatStringsSep "\n\n" (filter (s: s != "") (flatten [
                ";;; -*- lexical-binding: t; -*-"
                prelude
                (optional requireUsePackage ''
                  (eval-when-compile
                    (require 'use-package))'')
                (optional requireBindKey
                  "(require 'bind-key)")
                (mapAttrsToList mkPackageString newPkgs)
                "(provide '${pname})"
              ])));
              preBuild = ''
                emacs -Q --batch \
                  ${optionalString dependUsePackage ''--eval "(require 'use-package)"''} \
                  --eval '(find-file "${pname}.el")' \
                  --eval '(indent-region (point-min) (point-max))' \
                  --eval '(write-file "${pname}.el")'
                '';
            };
        mkInitDrvs = pairs:
          let
            go = pairs: oldPkgs:
              if pairs == [ ] then [ ] else
                let
                  hd = head pairs;
                  newPkgs = filterAttrs (_: pkg: pkg.enable) hd.opts.packages;
                  prelude = hd.opts.prelude;
                  tl = tail pairs;
                in
                  [ (mkInitDrv hd.name oldPkgs newPkgs prelude) ] ++ go tl (oldPkgs // newPkgs);
          in
            go pairs { };
      in
        mkInitDrvs [
          { name = "early-init"; opts = cfg.earlyInit; }
          { name = "init"; opts = cfg.init; }
        ];
    home.file.".config/emacs/early-init.el".text = ''
      (require 'emacs-early-init)
    '';
    home.file.".config/emacs/init.el".text = ''
      (require 'emacs-init)
    '';
  };
}
