package ${1:`(or (ngpc/maven-guess-package (buffer-file-name)) "mypackage")`};

class `(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))` {
    $0
}
