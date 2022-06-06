package ${1:`(or (ngpc/maven-guess-package (buffer-file-name)) "com.example.mypackage")`};

class `(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))` {
    $0
}
