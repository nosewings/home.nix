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
    programs.bash.initExtra = ''
      mvn-exec-java() {
          shift
          mvn exec:java -Dexec.mainClass=$1 $@
      }
    '';
    programs.zsh.initExtra = ''
      mvn-exec-java() {
          shift
          mvn exec:java -Dexec.mainClass=$1 $@
      }
    '';
    programs.fish.functions = {
      mvn-exec-java = {
        argumentNames = "mainClass";
        body = "mvn exec:java -Dexec.mainClass=$mainClass $argv[2..]";
      };
    };
    programs.emacs.init.init.packages = {
      cc-mode = {
        enable = true;
        package = null;
        preface = ''
          (defun ngpc/maven-root (file)
            (--when-let (locate-dominating-file file "pom.xml")
              (f-full it)))
          (defun ngpc/maven-source-root (file)
            (-when-let (mvn-root (ngpc/maven-root file))
              (--first (s-starts-with? (f-join mvn-root it) file)
                       '("src/main/java"
                         "src/main/resources"
                         "src/main/filters"
                         "src/main/webapp"
                         "src/test/java"
                         "src/test/resources"
                         "src/test/filters"
                         "src/it"
                         "src/assembly"
                         "src/site"
                         ""))))
          (defun ngpc/maven-guess-package (file)
            (--when-let (ngpc/maven-source-root file)
              (->> it
                   (f-relative file)
                   (f-parent)
                   (directory-file-name)
                   (s-replace "/" "."))))'';
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
