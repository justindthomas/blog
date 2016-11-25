# Updating OpenSolaris on XenServer 5

Following my previous post from December on installing OpenSolaris 2008.11 in a para-virtualized environment on XenServer 5, I thought it might be helpful for some folks to understand how to update their new virtual machines. A commenter on the original article (Thanks phrost!) discovered some of this. Since originally installing the 2008.11 release, I’ve switched to the development repository and I've been tracking the pre-release 2009.06 code.

When an "Update All" operation is performed on the OpenSolaris machine, a new Boot Environment (BE) is created. This new BE will be suffixed with a `-#` as an iteration to the original BE (e.g., an original `opensolaris` becomes `opensolaris-1` which would later become `opensolaris-2`, etc.).

Because the PV method described in my original post specifies the kernel, ramdisk, and boot arguments outside of the typical OS boot process, all of those items need to be updated to reflect the changes when a new boot environment is created. The steps to do this are fairly straightforward:

1. After updating, but before rebooting, copy the new kernel and ramdisk over to the XenServer. Determine the mountpoint of the new boot environment. This will typically be `/tmp/<random characters>`. Use `beadm list` to find that mountpoint. Change directories to `/tmp/<random characters>/platform`. Use `scp` to copy `i86xpv/kernel/unix` and `i86pc/boot_archive` over to the XenServer. You’ll place these in the `/opt/kernels` directory and you’ll probably want to name them something to uniquely identify them. Avoid writing over the currently configured kernel and ramdisk.
2. Re-configure the `PV-kernel`, `PV-ramdisk` and `PV-args` values of your OpernSolaris VM to point to the new files and the new BE. The change to `PV-args` will likely just be to specify the `-#` of the desired BE in the boot path.
3. Reboot the OpenSolaris VM and you should be running the new code!

One interesting tidbit: if you later do anything that modifies the `boot_archive` (e.g., installing the SMB server kernel modules), you'll want to copy the modified `boot_archive` over to the XenServer and modify `PV-ramdisk` accordingly. I learned that the hard way when I installed SMB, rebooted, and was greeted by all kinds of obtuse errors about the SMB service not starting.
