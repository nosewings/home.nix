{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.programs.emacs;
in
{
  options.ngpc.programs.emacs = {
    enable = mkEnableOption "Emacs config";
  };

  config = mkIf cfg.enable {
    programs.emacs.enable = true;
    home.packages = with pkgs; [ emacs-all-the-icons-fonts ];

    programs.emacs.vterm.enable = true;

    ngpc.programs.emacs.completion = "selectrum";
    ngpc.programs.emacs.circe.enable = true;
    ngpc.programs.emacs.lsp.enable = true;
    ngpc.programs.emacs.treemacs.enable = true;
    ngpc.programs.emacs.which-key.enable = true;

    programs.emacs.init = {
      enable = true;
      earlyInit = ''
        (setq gc-cons-threshold most-positive-fixnum)

        (push '(menu-bar-lines . 0) default-frame-alist)
        (push '(tool-bar-lines . 0) default-frame-alist)

        (setq menu-bar-mode nil
              tool-bar-mode nil)'';
      init = {
        prelude = readFile ./emacs/init/prelude.el;
        packages = {
          ace-window = {
            enable = true;
            bind = {
              "" = {
                "\"C-x o\"" = "ace-window";
              };
            };
          };
          company = {
            enable = true;
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
              dashboard-mode = [ "ngpc/dont-show-trailing-whitespace" ];
            };
          };
          doom-modeline = {
            enable = true;
            config = "(doom-modeline-mode)";
          };
          doom-themes = {
            enable = true;
            config = "(load-theme 'doom-outrun-electric t)";
          };
          flycheck = {
            enable = true;
            config = "(global-flycheck-mode)";
          };
          gcmh = {
            enable = true;
            hook = {
              window-setup = [ "gcmh-mode" ];
            };
          };
          git-gutter = {
            enable = true;
            config = "(global-git-gutter-mode)";
          };
          highlight-numbers = {
            enable = true;
          };
          magit = {
            enable = true;
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
                "\"C-c o r a a\"" = "org-roam-alias-add";
                "\"C-c o r f\"" = "org-roam-node-find";
                "\"C-c o r i\"" = "org-roam-node-insert";
                "\"C-c o r s\"" = "org-roam-db-sync";
                "\"C-c o r t\"" = "org-roam-buffer-toggle";
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
          restart-emacs = {
            enable = true;
          };
          vterm = {
            enable = true;
            hook = {
              vterm-mode = [ "ngpc/dont-show-trailing-whitespace" ];
            };
          };
          ws-butler = {
            enable = true;
            config = "(ws-butler-global-mode)";
          };
        };
      };
    };
  };
}
