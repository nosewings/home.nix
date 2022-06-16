{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.ngpc.languages.java;
in
{
  options.ngpc.languages.java = {
    enable = mkEnableOption "Java config";
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      openjdk11
      maven
    ];
    programs.git.ignores = [
      "*.class"
      "*.jar"
    ];
    programs.emacs.init.init.packages = {
      cc-mode = {
        enable = true;
        package = null;
        preface = ''
          (defun ngpc/maven-root (file)
            (f-full (locate-dominating-file file "pom.xml")))
          (defun ngpc/maven-source-root (file)
            (-if-let (mvn-root (f-full (ngpc/maven-root file)))
              (->> '("src/main/java"
                     "src/main/resources"
                     "src/main/filters"
                     "src/main/webapp"
                     "src/test/java"
                     "src/test/resources"
                     "src/test/filters"
                     "src/it"
                     "src/assembly"
                     "src/site"
                     "")
                   (--map (f-join mvn-root it))
                   (--first (s-starts-with? it file)))))
          (defun ngpc/maven-guess-package (file)
            (-some->> (ngpc/maven-source-root file)
                      (f-relative file)
                      (f-parent)
                      (directory-file-name)
                      (s-replace "/" ".")))'';
      };
      lsp-java = {
        enable = true;
        hook = {
          java-mode = [ "lsp-deferred" ];
        };
      };
    };
    ngpc.programs.emacs.yatemplate.templateFiles = {
      ":.*.java" = ./templates/template.java;
      "0:pom.xml" = ./templates/template.pom.xml;
    };
  };
}
