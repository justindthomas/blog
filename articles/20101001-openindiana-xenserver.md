# OpenIndiana and XenServer

Now that OpenSolaris is officially dead and the drama has died down a bit, I thought it might be time to figure out how to install OpenIndiana on my XenServer. I've heard some say that you can't install the new distribution on Xen as a domU, but that is demonstrably incorrect (although as I understand it, dom0 is presently out of the question due to Oracle pulling out the relevant xVM bits).

Installing OpenIndiana 147 on XenServer 5.5 is nearly identical to installing OpenSolaris on the same (as would be expected considering OpenIndiana’s roots).

* Create a new VM using the "Other Install Media" profile within XenCenter. Set up the VM with 10GB of disk space and 1GB of RAM.
* Copy the `/platform/i86xpv/kernel/amd64/unix` and `/platform/i86pc/amd64/boot_archive` files from the OI install disk over to the XenServer host.
* On the XenServer, determine the UUID of the newly created OpenIndiana VM using `xe vm-list`; just note the first 3 or 4 characters and tab completion will enter the rest when necessary.
* Configure the following parameters:

~~~~ {.bash}
xe vm-param-set uuid=<vm uuid> PV-kernel=<full path to the 'unix' file on the XenServer>
xe vm-param-set uuid=<vm uuid> PV-ramdisk=<full path to the 'boot_archive' file on the XenServer>
xe vm-param-set uuid=<vm uuid> PV-args='/platform/i86xpv/kernel/amd64/unix -B console=ttya'
xe vm-param-set uuid=<vm uuid> HVM-boot-policy=
xe vm-param-set uuid=<vm uuid> PV-bootloader=
~~~~

* Mount the OpenIndiana install CD in the appropriate drive (e.g., select the correct ISO in XenCenter)
* Boot the OpenIndiana VM. Log in as jack/jack when appropriate to do so.
* Configure basic networking if needed; my Windows 2008 DHCP server never works to assign addresses to OpenSolaris/OpenIndiana (for whatever reason) so this is generally a mandatory step.
* Connect to the OpenIndiana server with an SSH client with X-tunneling enabled using the jack/jack account.
* Execute: `pfexec /usr/bin/gui-install`. The graphical install process will begin. Complete the steps as requested.
* After the installation is completed (and before rebooting), change the PV-args on the XenServer to: `xe vm-param-set uuid=<vm uuid> PV-args='/platform/i86xpv/kernel/amd64/unix -B console=ttya,zfs-bootfs=rpool/ROOT/openindiana,bootpath="/xpvd/xdf@51728:a"'`. Note the two changes from the OpenSolaris instructions from an earlier blog post: the zfs-bootfs is openindiana, not opensolaris and the bootpath is 51728 instead of 51712. I have no idea why the latter change is necessary - I just know that there was no 51712 in my devices directory, only 51728 and a 51760.

Reboot, and you're good to go! Remember to run `bootadm update-archive` after the first boot (and anytime you make changes that require that file to be updated) and copy out the updated `/platform/i86pc/amd64/boot_archive` to the XenServer before rebooting.

Here's a screenshot; note that I don't know what's up with the savecore error. Considering the unstable nature of the code, those sorts of hiccoughs don't surprise me.

![OpenIndiana](https://ser.endipito.us/file/oi.png)
