# Installing NexentaCP 2 RC1 on XenServer 5

Although Nexenta uses the OpenSolaris kernel, there are a few unique steps that you'll need to take in order to get Nexenta up and running paravirtualized (PV) on XenServer 5. These steps are a result of the lack of certain files in the default ramdisk on the Nexenta installation CD.

Also, the CDROM device seems to be handled differently in Nexenta than it is in OpenSolaris. As a result, these instructions will require you to fully install Nexenta in HVM mode and then flip the right bits to convert it to PV.

A few notes: I use `/opt/kernels` on my XenServer to store the kernels and ramdisks I use for my PV systems. You can substitute whatever you like for that directory; technically I think the correct place would be somewhere in `/usr/local`. Likewise, the names that I give the ramdisks and kernels are completely subjective; feel free to devise your own scheme.

* Using the "Other Media" option in XenCenter, install NexentaCP from the installation CD like you’d install a Windows system.
* Configure the installation appropriately at the presented prompts. Note that due to running in HVM mode, this initial installation will take a significant amount of time. Fear not, things will move much more quickly by the time you’re done.
* Copy `/platform/i86pc/miniroot` from the Nexenta installation CD to a system where you can work with it. I used an OpenSolaris system for the next steps.
* Rename `miniroot` to `miniroot.gz`.
* Decompress the `miniroot.gz` archive: `gunzip miniroot.gz`
* Mount the miniroot archive as a loopback device:

~~~~ {.bash}
mount -o loop [/path/to/]miniroot /[mountpoint]
~~~~

* Change directories into the `/platform` directory on the mounted filesystem
* Copy the entire `/platform/i86xpv` directory from the HVM Nexenta system via scp over to the mounted filesystem. You might need to do this from the HVM Nexenta system (unless you `svcadm enable ssh` on the HVM Nexenta system).
* Move out of the miniroot filesystem on your working machine (the OpenSolaris box for me) and unmount the archive:

~~~~ {.bash}
unmount /[mountpoint]
~~~~

* Recompress the miniroot archive: `gzip miniroot`
* Copy the newly modified miniroot archive over to an appropriate directory on the XenServer:

~~~~ {.bash]
scp ./miniroot.gz
root@xenserver:/opt/kernels/ramdisk_nexenta_install
~~~~

* Log in to the Nexenta HVM system and copy the i86xpv kernel over to the XenServer:

~~~~ {.bash}
scp /platform/i86xpv/kernel/unix
root@xenserver:/opt/kernels/kernel_nexenta
~~~~

* Log in to the XenServer and configure the Nexenta VM to use the kernel and ramdisk you've moved over:

~~~~ {.bash}
xe vm-param-set uuid=[VM UUID]
PV-kernel=/opt/kernels/kernel_nexenta
xe vm-param-set uuid=[VM UUID] PV-ramdisk=
/opt/kernels/ramdisk_nexenta_install
xe vm-param-set uuid=[VM UUID]
PV-args='/platform/i86xpv/kernel/unix
-B console=ttya -m milestone=0 -v'
xe vm-param-set uuid=[VM UUID]
HVM-boot-policy=
~~~~

* Reboot the newly paravirtualized Nexenta VM. It will fail to boot - that's okay.
* When prompted to log in for maintenance, enter root with a blank password. If you have trouble getting the VM to respond to your keystrokes in XenCenter, try restarting XenCenter; every time I've done this, XenCenter has failed at this point and had to be restarted.
* Import the syspool zpool: `zpool import -f syspool`
* Configure the PV-args on the XenServer to specify the bootfs:

~~~~ {.bash}
xe vm-param-set uuid=[VM UUID] PV-args=
'/platform/i86xpv/kernel/unix -B console=ttya,
zfs-bootfs=syspool/rootfs-nmu-000,
bootpath="/xpvd/xdf@51712:a"'
~~~~

* Reboot your Nexenta system. At this point, it should boot up and let you log in normally.
* Once booted, plumb your virtualized network interfaces: `ifconfig xnf0 plumb`
* Rename `/etc/hostname.rtls0` and `/etc/hostname6.rtls0` to `/etc/hostname.xnf0` and `/etc/hostname6.xnf0`
* Reboot.
* Copy `/platform/i86pc/boot\_archive` from the booted Nexenta system over to the XenServer as something like `/opt/kernels/ramdisk_nexenta`.
* Reconfigure the VM parameters to point at the new boot archive:

~~~~ {.bash}
xe vm-param-set uuid=[VM UUID]
PV-ramdisk=/opt/kernels/ramdisk_nexenta
~~~~

* Reboot.

Congratulations! You now have a paravirtualized Nexenta core system. Upon booting, the VM screen should look something like:

~~~~ {.bash}
v3.2.1 chgset ’58bf50a2c754.3c18e9e0f827 (3.2.1 5.0.0.235.17085)’
SunOS Release 5.11 Version NexentaOS_20081207 32-bit
Loading Nexenta…
NOTICE: xdf@51712: failed to read feature-barrier
Hostname: nexenta-test
Reading ZFS config: done.
Mounting ZFS filesystems: (2/2)

NexentaCore 2.0 RC1 (Hardy 8.04/b104+)

nexenta-test console login:
~~~~

Take particular note: when you update a package that includes a kernel module, be sure to update the `boot\_archive` (`bootadm update-archive`) and copy that updated archive over to the XenServer as `/opt/kernels/ramdisk_nexenta` BEFORE rebooting the Nexenta VM. I ran into a problem where the system could not load the console after doing an `aptitude safe-upgrade` and rebooting without updating the ramdisk.

Also, after installing I had to run an `apt-get -f install` to finish the installation of `libtimedate-perl` before doing an `aptitude safe-upgrade`. That package doesn’t appear to be installed correctly by the installer.

As always, please let me know if you have any comments or suggestions!
