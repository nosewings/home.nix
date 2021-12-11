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
        type = bool;
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
      globalBinds = bind."" or "";
      nonGlobalBinds = removeKey "" bind;
      mkBindings = bindings: concatStringsSep "\n" (mapAttrsToList (key: bind: "(${key} . ${bind})") bindings);
      mkNonGlobalBindings = keymap: bindings: ":map ${keymap}\n" + mkBindings bindings;
    in
      ":${keyword} (${concatStringsSep "\n" (filter (x: x != "") [
        (mkBindings globalBinds)
        (concatStringsSep "\n" (mapAttrsToList mkNonGlobalBindings nonGlobalBinds))
      ])})"
  );

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

  mkDefer = defer: optionalString defer
    ":defer ${boolToElisp defer}";

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
in
{
  options.programs.emacs.init = {
    enable = mkEnableOption "Emacs init files";

    earlyInit = mkOption {
      type = lines;
      default = "";
    };

    init = {
      prelude = mkOption {
        type = lines;
        default = "";
      };

      packages = mkOption {
        type = attrsOf packageType;
        default = {};
      };
    };
  };

  config = mkIf cfg.enable {
    programs.emacs.enable = true;
    programs.emacs.extraPackages = epkgs:
      let
        enabledPackages = filterAttrs (_: v: v.enable) cfg.init.packages;
        hasPackages = enabledPackages != {};
        hasBinds = any (pkg: pkg.bind != {} || pkg.bind-keymap != {}) (attrValues enabledPackages);
        dependencies =
          optional hasPackages epkgs.use-package ++ optional hasBinds epkgs.bind-key ++ concatLists (mapAttrsToList (_: v: optional (v.package != null) (v.package epkgs)) enabledPackages);
        mkInitPackage = baseName: packages: srcParts:
          let
            rawSrc = concatStringsSep "\n\n" (filter (part: part != "") srcParts);
          in
            epkgs.trivialBuild rec {
              pname = "emacs-${baseName}";
              src = pkgs.writeText "${pname}.el" rawSrc;
              packageRequires = packages;
              preBuild = ''
                emacs -Q --batch \
                  --eval '(find-file "${pname}.el")' \
                  --eval '(let ((indent-tabs-mode nil) (lisp-indent-offset 2)) (indent-region (point-min) (point-max)))' \
                  --eval '(write-file "${pname}.el")'
              '';
            };
        emacs-early-init = mkInitPackage "early-init" [] [
          ";;; -*- lexical-binding: t; -*-"
          cfg.earlyInit
          "(provide 'emacs-early-init)"
        ];

        emacs-init = mkInitPackage "init" dependencies [
          ";;; -*- lexical-binding: t; -*-"
          cfg.init.prelude
          (optionalString hasPackages ''
            (eval-when-compile
              (require 'use-package))'')
          (optionalString hasBinds "(require 'bind-key)")
          (concatStringsSep "\n\n" (mapAttrsToList mkPackageString enabledPackages))
          "(provide 'emacs-init)"
        ];
      in
        [ emacs-early-init emacs-init ];

    home.file.".config/emacs/early-init.el".text = ''
      (require 'emacs-early-init)
    '';
    home.file.".config/emacs/init.el".text = ''
      (require 'emacs-init)
    '';
  };
}
