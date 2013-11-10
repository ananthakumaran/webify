# Webify

A command line tool to convert ttf file to woff, eot & svg files

## Usage

    $> webify fontname.ttf

## Binaries

[mac 64 bit](https://sourceforge.net/projects/webify/files/mac/webify/download)

[linux 64 bit](https://sourceforge.net/projects/webify/files/linux/webify/download)

[linux 32 bit](https://sourceforge.net/projects/webify/files/linux32/webify/download)

[windows 32 bit](https://sourceforge.net/projects/webify/files/windows/webify.exe/download)

[old versions](https://sourceforge.net/projects/webify/files/)


## CSS @font-face template

````css
    @font-face {
        font-family: 'my-font-family';
        src: url('my-font-filename.eot');
        src: url('my-font-filename.eot?#iefix') format('embedded-opentype'),
        url('my-font-filename.svg#my-font-family') format('svg'),
        url('my-font-filename.woff') format('woff'),
        url('my-font-filename.ttf') format('truetype');
        font-weight: normal;
        font-style: normal;
    }
````
