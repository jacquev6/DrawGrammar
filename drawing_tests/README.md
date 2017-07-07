How to setup visual diff for images
===================================

In .git/config:

    [diff "image"]
        command = .git/diff-image.sh

In .git/info/attributes:

    *.png diff=image

Install imagemagick.

File .git/diff-image.sh (make executable):

    #!/bin/bash

    compare $2 $1 png:- | montage -geometry +4+4 $2 - $1 png:- | display -title "$1" -
