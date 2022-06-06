{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs;
  normalModes = [ "conf-mode" "prog-mode" "text-mode" ];
  normalModeHooks = zipAttrsWith (name: values: head values) (forEach normalModes (mode: {
    "${mode}" = {
      enable = true;
      package = null;
      hook = {
        "${mode}" = [
          "display-line-numbers-mode"
          "ngpc/enable-show-trailing-whitespace"
        ];
      };
    };
  }));
in
{
  options.ngpc.programs.emacs = {
    enable = mkEnableOption "Emacs config";
  };

  config = mkIf cfg.enable {
    programs.emacs.enable = true;
    programs.emacs.overrides = self: super: {
      explain-pause-mode = self.trivialBuild {
        pname = "explain-pause-mode";
        version = "0.1";
        src = pkgs.fetchFromGitHub {
          owner = "lastquestion";
          repo = "explain-pause-mode";
          rev = "2356c8c3639cbeeb9751744dbe737267849b4b51";
          sha256 = "++znrjiDSx+cy4okFBBXUBkRFdtnE2x+trkmqjB3Njs=";
        };
      };
    };

    ngpc.programs.emacs.all-the-icons.enable = true;
    ngpc.programs.emacs.completion = "vertico";
    ngpc.programs.emacs.circe.enable = true;
    ngpc.programs.emacs.lsp.enable = true;
    ngpc.programs.emacs.magit.enable = true;
    ngpc.programs.emacs.projectile.enable = true;
    ngpc.programs.emacs.treemacs.enable = true;
    ngpc.programs.emacs.vterm.enable = true;
    ngpc.programs.emacs.which-key.enable = true;
    ngpc.programs.emacs.yatemplate.enable = true;

    programs.emacs.init = {
      enable = true;
      earlyInit = ''
        (setq gc-cons-threshold most-positive-fixnum)

        (push '(menu-bar-lines . 0) default-frame-alist)
        (push '(tool-bar-lines . 0) default-frame-alist)

        (setq menu-bar-mode nil
              tool-bar-mode nil)'';
      init = {
        earlyPackages = {
          dash = {
            enable = true;
            no-require = false;
          };
          f = {
            enable = true;
            no-require = false;
          };
          s = {
            enable = true;
            no-require = false;
          };
        };
        prelude = readFile ./init/prelude.el;
        packages = mkMerge [
          normalModeHooks
          {
            beacon = {
              enable = true;
              config = "(beacon-mode)";
              custom = {
                beacon-blink-when-focused = "t";
              };
            };
            company = {
              enable = true;
            };
            company-box = {
              enable = true;
              custom = {
                "company-box-doc-delay" = "0";
              };
              hook = {
                company-mode = [ "company-box-mode" ];
              };
            };
            dashboard = {
              enable = true;
              config = "(dashboard-setup-startup-hook)";
              custom = {
                dashboard-center-content = "t";
                dashboard-startup-banner = "'logo";
                dashboard-set-init-info = "t";
              };
              hook = {
                after-init = [ "dashboard-refresh-buffer" ];
              };
            };
            doom-modeline = {
              enable = true;
              config = "(doom-modeline-mode)";
            };
            doom-themes = mkMerge [
              {
                enable = true;
                config = "(load-theme 'doom-outrun-electric t)";
              }
              (mkIf cfg.treemacs.enable {
                config = "(doom-themes-treemacs-config)";
                custom = {
                  doom-themes-treemacs-enable-variable-pitch = "nil";
                  doom-themes-treemacs-theme = "\"doom-colors\"";
                };
              })
            ];
            explain-pause-mode = {
              enable = false;
              config = "(explain-pause-mode)";
              no-require = false;
            };
            flycheck = {
              enable = true;
              config = "(global-flycheck-mode)";
            };
            font-lock-studio = {
              enable = true;
            };
            gcmh = {
              enable = true;
              custom = {
                gcmh-idle-delay = "1";
              };
              hook = {
                window-setup = [ "gcmh-mode" ];
              };
            };
            git-gutter = {
              enable = true;
              config = "(global-git-gutter-mode)";
            };
            highlight-indent-guides = {
              enable = true;
              custom = {
                highlight-indent-guides-method = "'character";
              };
              hook = {
                prog-mode = [ "highlight-indent-guides-mode" ];
              };
            };
            highlight-numbers = {
              enable = true;
              hook = {
                prog-mode = [ "highlight-numbers-mode" ];
              };
            };
            hl-todo = {
              enable = true;
              config = "(global-hl-todo-mode)";
            };
            lorem-ipsum = {
              enable = true;
              bind = {
                "" = {
                  "\"C-c i l p\"" = "lorem-ipsum-insert-paragraphs";
                  "\"C-c i l s\"" = "lorem-ipsum-insert-sentences";
                  "\"C-c i l l\"" = "lorem-ipsum-insert-list";
                };
              };
            };
            nxml-mode = {
              enable = true;
              package = null;
              bind = {
                nxml-mode-map = {
                  "\"C-c C-e\"" = "ngpc/nxml-insert-tag";
                };
              };
              preface = ''
                (defun ngpc/nxml-insert-tag (tag &optional arg)
                  (interactive "MTag: \nP")
                  (let ((start (point)))
                    (insert "<" tag ">")
                    (unless arg (newline))
                    (let ((end (save-excursion
                                 (unless arg (newline))
                                 (insert "</" tag ">")
                                 (point))))
                      (indent-region start end)
                      (indent-for-tab-command))))'';
            };
            nyan-mode = {
              enable = true;
              config = "(nyan-mode)";
            };
            org = {
              enable = true;
              bind = {
                "" = {
                  "\"C-c o s l\"" = "org-store-link";
                };
              };
              hook = {
                org-mode = [ "auto-fill-mode" ];
              };
              preface = ''
                (defun ngpc/org-raw-link-at-point ()
                  (let ((ctx (org-element-context)))
                    (if (not (eq (org-element-type ctx) 'link))
                        (user-error "No link found")
                      (org-element-property :raw-link ctx))))

                (defun ngpc/org-copy-raw-link-at-point ()
                  (interactive)
                  (kill-new (ngpc/org-raw-link-at-point)))'';
            };
            org-roam = {
              enable = true;
              bind = {
                "" = {
                  "\"C-c o r a\"" = "org-roam-alias-add";
                  "\"C-c o r b\"" = "org-roam-buffer-toggle";
                  "\"C-c o r d\"" = "org-roam-db-sync";
                  "\"C-c o r f\"" = "org-roam-node-find";
                  "\"C-c o r i\"" = "org-roam-node-insert";
                  "\"C-c o r t\"" = "org-roam-tag-add";
                };
              };
              custom = {
                org-roam-directory = "\"~/org/roam\"";
              };
              init = "(setq org-roam-v2-ack t)";
              config = "(org-roam-setup)";
              preface = ''
                (defun ngpc/org-roam-load-directory (dir)
                  (interactive "DLoad directory: ")
                  (setq org-roam-directory dir)
                  (org-roam-db-sync))'';
            };
            pkg-info = {
              enable = true;
            };
            prog-mode = {
              enable = true;
              package = null;
              hook = {
                prog-mode = [ "ngpc/enable-show-trailing-whitespace" ];
              };
            };
            restart-emacs = {
              enable = true;
            };
            savehist = {
              enable = true;
              package = null;
              config = "(savehist-mode)";
            };
            solaire-mode = {
              enable = true;
              config = "(solaire-global-mode)";
            };
            unicode-fonts = {
              enable = false;
              config = "(unicode-fonts-setup)";
            };
            winum = {
              enable = true;
              config = "(winum-mode)";
            };
            ws-butler = {
              enable = true;
              config = "(ws-butler-global-mode)";
            };
          }
        ];
      };
    };
  };
}
