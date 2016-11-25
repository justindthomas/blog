# XenServer 5 and OpenSolaris 2008.11

I really enjoy the XenServer product from Citrix (formerly XenSource).  It's relatively lightweight, self-contained, works with my hardware, provides para-virtualized drivers for Windows and is free.  The latest revision (v5 as of this posting) has also eliminated many of the crippling aspects of the v4 "Express" series.

However, perhaps the most frustrating limitation of XenServer is its lack of support for non-Linux/Windows operating systems.  Some operating systems can be shoe-horned in as HVM guests, but performance is nothing short of abysmal.

OpenSolaris is one system that I've yearned to have XenServer properly support.  Either my desires towards that OS are uncommon or the challenge of properly configuring XenServer to support this operating system has stumped most folks; I have found very little in the way of instruction to support such a goal.

Over this last weekend I was relegated to being a shut-in due to an unusually prolific snow storm that occurred (and continues to occur) here in Portland, Oregon.  I took the opportunity to solve this OpenSolaris/XenServer issue once and for all.  Follow the proceeding instructions to install OpenSolaris 2008.11 on XenServer 5 as a para-virtualized guest.

Create a new VM using the "Other Install Media" profile.  Set up your disks and network interfaces as you’d like. Be sure to select enough memory: 1024MB works fine, 512MB boots but is relatively unresponsive, 256MB generates a kernel panic. Don’t actually start the VM yet.

Copy two files from the OpenSolaris install CD to a directory on your XenServer.  I chose `/opt/kernels`, but you could use whatever you want: `/boot/x86.microroot` and `/platform/i86xpv/kernel/unix`.  We’ll assume you kept the same names (`x86.microroot` and `unix`) for later configuration items.  Technically, you can name them whatever you want.

On the XenServer, determine the UUID of the newly created OpenSolaris VM. You just need to remember the first 3 or 4 characters; tab completion works nicely for this.

~~~~ {.bash}
xe vm-list
~~~~

Point the VM's `PV-kernel` value at the kernel stored on the XenServer.  Using our previously established values that would be: 

~~~~ {.bash}
xe vm-param-set uuid=<vm uuid> PV-kernel='/opt/kernels/unix'
~~~~

Point the VM’s PV-ramdisk value at the ramdisk stored on the XenServer.  Using our previously established values that would be:

~~~~ {.bash}
xe vm-param-set uuid=<vm-uuid> PV-ramdisk='/opt/kernels/x86.microroot'
~~~~

Configure the VM’s PV-args.  Execute:

~~~~ {.bash}
xe vm-param-set uuid=<vm uuid> PV-args='/platform/i86xpv/kernel/unix -B console=ttya'
~~~~

Clear the HVM boot policy to force the VM into PV mode:

~~~~ {.bash}
xe vm-param-set uuid=<vm uuid> HVM-boot-policy=
~~~~

Ensure the boot loader value is clear:

~~~~ {.bash}
xe vm-param-set uuid=<vm uuid> PV-bootloader=
~~~~

Set the VBD `bootable` setting to true. Use `xe vbd-list` to find the VBD UUID associated with your new VM. Note that this may not be strictly necessary; I’ve skipped this step with no trouble.

~~~~ {.bash}
xe vbd-param-set uuid=<vbd uuid> bootable=true
~~~~

Mount the OpenSolaris install CD in the appropriate drive (e.g., select the right ISO).

Boot the OpenSolaris VM.  The install CD should start up and prompt you for a couple of language and region settings.

Log in as `jack` using the password `jack`.

Note your IP address (it’s configured by DHCP): `ifconfig -a`

Configure the vncserver:

~~~~ {.bash}
mkdir ~/.vnc; cp ~/.Xclients ~/.vnc/xstartup
~~~~

Start the vncserver and select a password. You might need to do this twice: start it once to set the password, kill it and then start it again in the background so that you can interact with the server from your terminal (i.e., `vncserver &`): `vncserver`

Using a VNC client, connect to your OpenSolaris instance on port 5901.

From your terminal window, you’ll probably need to kill twm and start gnome-session.

Just double-click the "Install OpenSolaris" icon to complete the installation.

Set the PV-args on the XenServer to point at your newly installed OS. I suspect that the `opensolaris` in `rpool/ROOT/opensolaris` refers to your boot environment, which is based on your host name; my host name happens to be `opensolaris`. You might also need an iteration of that value if you have multiple BE checkpoints (e.g., `opensolaris-1`, `opensolaris-2`, etc.) As I've updated my system, I’ve found it necessary to also update this value. Keep that in mind if you run into trouble:

~~~~ {.bash}
xe vm-param-set uuid=<vm uuid> PV-args='/platform/i86xpv/kernel/unix -B console=ttya,zfs-bootfs=rpool/ROOT/opensolaris,bootpath="/xpvd/xdf@51712:a"'
~~~~

Be sure your install CD is unmounted and boot up your new system.

Congratulations!  You now have an OpenSolaris system running in PV mode on your XenServer.  Unfortunately there are no Xen tools available (that I’m aware of), but basic system performance should be much better than the HVM alternative.

Xvnc is installed by default; you’ll probably want to configure and start that service to permit ongoing management of the system. Configure that service in this way:

Create a user and a group for the Xvnc service:

~~~~ {.bash}
pfexec groupadd xvnc
pfexec useradd -d / -g xvnc -s /usr/bin/pfsh xvnc
~~~~

Set the VNC password:

~~~~ {.bash}
pfexec vncpasswd /etc/X11/.vncpasswd
~~~~

Protect the VNC password file:

~~~~ {.bash}
pfexec chown xvnc /etc/X11/.vncpasswd
pfexec chgrp xvnc /etc/X11/.vncpasswd
pfexec chmod 400 /etc/X11/.vncpasswd
~~~~

Configure the service properties:

~~~~ {.bash}
svccfg -s application/x11/xvnc_inetd
setprop inetd_start/exec = astring: "/usr/X11/bin/Xvnc -inetd -query localhost -once -securitytypes vncauth -rfbauth /etc/X11/.vncpasswd"
setprop inetd_start/user = astring: xvnc
setprop inetd_start/group = astring: xvnc
~~~~

Stop and restart the Xvnc service:

~~~~ {.bash}
svcadm disable xvnc-inetd gdm
svcadm enable xvnc-inetd gdm
~~~~

Suggestions on better ways to do any of this are welcome!

*UPDATE*: To learn how to update a paravirtualized OpenSolaris VM on XenServer to a newer kernel or modified Boot Environment, see my updated blog entry.
