# How to get Pharo running on WSL

Pharo can be run under Windows 10 Subsystem for Linux (WSL).

- Install WSL (see [microsoft documentation](https://docs.microsoft.com/en-us/windows/wsl/install-win10))
- Install X Server for Windows (see [this blogpost](https://jaipblog.wordpress.com/2018/01/21/running-linux-gui-apps-on-windows-10/), I used [VcXsrv](https://sourceforge.net/projects/vcxsrv/)).
- Install [Mesa](https://wiki.debian.org/Mesa) with `sudo apt install mesa-utils` (needed because there are missing libraries for the x11 display used by Pharo).
> Note: The Mesa dependency may be overkill. On the [Pharo install page](https://pharo.org/web/gnu-linux-installation-64) there's mention of some things to do for Ubuntu 16. 
- Install Pharo:

```bash
$ mkdir MyPharo
$ cd MyPharo/
$ curl -L https://get.pharo.org/64/ | bash
```

The last step can also be the Pharo Launcher.

> Note: Last time it has been tested, only Pharo of version >= 6 64bit could be run.
> Pharo 32bits is not compatible because of a missing part of the kernel.

For other Pharo/WSL issues, see [this blog](https://fuhrmanator.github.io/2019/02/27/Pharo-in-WSL.html). 
