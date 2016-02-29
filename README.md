# mathematica-addons
Some useful Mathematica addons

## Installation Instructions

Install the files in the addons package into a directory where **Mathematica** can find them.
For example, my **Mathematica** installation is in

>   /usr/local/apps/mathematica/

so I put the files in

>   /usr/local/apps/mathematica/AddOns/Applications

If you don't have root access, put them in another directory, such as

>   $HOME/.Mathematica/Applications

then set the Mathematica `$Path` variable so the kernel can find them.
For example, to alert Mathematica to the files in the directory
`$HOME/share/math`, put the following line in the `$HOME/.Mathematica/Kernel/init.m` file:
```
$Path = Append[$Path, ToFileName[{$HomeDirectory, "share", "math"}]];
```

In the file `init.m` included with this distribution is a series of `DeclarePackage` declarations which, if you wish, can also be put into the `.Mathematica/(VERSION)/Kernel/init.m` file to autoload the packages.

Enjoy!

Aaron A. King
<kingaa at umich dot edu>
