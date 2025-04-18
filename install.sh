read -r -d '' DISCLAIMER <<'EOF'

  #############################################################################
  #                                                                           #
  #   Brielle's (Arch Linux) install script v0.0.1                            #
  #                                                                           #
  #   This is an installation script for Arch Linux.                          #
  #   It is designed for my personal use, but is available publicly for a     #
  #   couple of reasons. (curling a file is marginally more conveinient       #
  #   than transferring by usb, for one)                                      #
  #                                                                           #
  #   This script is an installer and will, even when operating as intended,  #
  #   make substantial changes the system it is being run on.                 #
  #                                                                           #
  #   While the script does provide some input options, (such as prompting    #
  #   for passwords) this script is tailored to my use case. I recommend      #
  #   reviewing the configuration variables in the script prior to running    #
  #   especially if you are not me.                                           #
  #                                                                           #
  #   DO NOT RUN THIS SCRIPT WITHOUT KNOWING WHAT IT DOES,                    #
  #   ESPECIALLY NOT IN A REAL NON-VM ENVIRONMENT.                            #
  #                                                                           #
  #   THIS SCRIPT MAY STEAMROLL YOUR OPERATING SYSTEM.                        #
  #   IN FACT, IT IS DESIGNED TO STEAMROLL YOUR OPERATING SYSTEM IF TOLD TO.  #
  #                                                                           #
  #############################################################################

EOF

### BEGIN DEFAULT CONFIG

D_GIT_REMOTE_URL='https://github.com/BrickNumber5/myconfig.git'
D_GIT_TARGET_LOCAL_DIR='.config/myconfig'

D_USERNAME='brielle'
D_HOSTNAME='compassion'

D_TIMEZONE='America/Denver'

D_ROOT_MNT='/mnt'

# Packages installed early during the installation process.
# The rest of myconfig/pkglist is installed later.
BASIC_PKG_LIST='linux linux-lts linux-firmware vim base-devel sof-firmware alsa-firmware alsa-utils alsa-lib networkmanager wireless_tools netctl wpa_supplicant dialog lvm2 grub efibootmgr dosfstools os-prober mtools git'
# Hardware specific packages (Which you want if and only if they are correct for your hardware)
HW_SPEC_PKG_LIST='intel-ucode nvidia-open nvidia-open-lts mesa'

PREFIX='INSTALL SCRIPT'

### END DEFAULT CONFIG

ask_yn() {
  while :; do
    printf "$1"
    read _yn
    case "$_yn" in
      [Yy]*) return 0 ;;
      [Nn]*) return 1 ;;
      "") case "$2" in
        [Yy]) return 0 ;;
        [Nn]) return 1 ;;
      esac
    esac
  done
}

read_defaulted() {
  read _val
  [ "$_val" = "" ] && _val="$2"
  read -r -d '' "$1" <<EOF
  $_val
EOF
}

printf "\n  %s\n\n" "$DISCLAIMER"

confirm_msg="yesruninstaller"
printf "Are you sure you want to run this script? Type '%s' to confirm:\n" "$confirm_msg"
read confirm_msg_check

[ "$confirm_msg" != "$confirm_msg_check" ] && exit 1

printf "Beginning Installer...\n"

if ask_yn "Would you like to reformat from scratch? (y/n) "; then
  phase0mode="fresh"
else
  phase0mode="atop"
fi

printf "\nPreflight Checklist:\n"
ask_yn "  Have you connected to the internet? (Y/n) " y || exit 1
ask_yn "  Have you ensured the install medium is up to date? (Y/n) " y || exit 1

if [ "$phase0mode" = "atop" ]; then
  ask_yn "  Have you set the system clock? (Y/n) " y || exit 1
  ask_yn "  Have you created desired partitions? (Y/n) " y || exit 1
  ask_yn "  Have you formated desired filesystems? (Y/n) " y || exit 1
  ask_yn "  Do you know the device name of your EFI boot partition? (Y/n) " y || exit 1
  ask_yn "  Have you mounted your filesystems? (Y/n) " y || exit 1
else
  ask_yn "  Have you backed up anything important? (Y/n) " y || exit 1
fi

printf "\n"

printf "What do you want the hostname to be? ($D_HOSTNAME) "
read_defaulted hostname "$D_HOSTNAME"
printf "What do you want the normal user to be called? ($D_USERNAME) "
read_defaulted username "$D_USERNAME"
printf "What timezone do you want to use? ($D_TIMEZONE) "
read_defaulted timezone "$D_TIMEZONE"

printf "Current mirrorlist:\n"
cat /etc/pacman.d/mirrorlist
ask_yn "Do these mirrors look okay? (Y/n) " y || exit 1

if [ "$phase0mode" = "fresh" ]; then
  printf "TODO: set clock\n"
  
  printf "TODO: partition\n"
  printf "TODO: format\n"
  printf "TODO: mount\n"
  
  root_mnt="$D_ROOT_MNT"
else
  printf "Where is the file system root mounted? ($D_ROOT_MNT) "
  read_defaulted root_mnt "$D_ROOT_MNT"
  
  printf "[$PREFIX] Deleting everything in $root_mnt not in $root_mnt/home\n"
  find "$root_mnt" -mindepth 1 -maxdepth 1 ! -name 'home' -exec rm -r {} +
  
  printf "[$PREFIX] Moving everything in $root_mnt/home to $root_mnt/home/bak\n"
  bak_temp_name='bak0'
  while [ -e "$root_mnt/home/$bak_temp_name" ]; do
    bak_temp_name="$bak_temp_name"'0'
  done
  mkdir "$root_mnt/home/$bak_temp_name"
  mv "$root_mnt/home/"* "$root_mnt/home/$bak_temp_name" 2>/dev/null
  mv "$root_mnt/home/$bak_temp_name" "$root_mnt/home/bak"
fi

printf "[$PREFIX] Generating /etc/fstab...\n"
mkdir -p "$root_mnt/etc"
genfstab -U -p "$root_mnt" >> "$root_mnt/etc/fstab"
cat "$root_mnt/etc/fstab"

printf "[$PREFIX] pacstrapping...\n"
pacstrap -i "$root_mnt" base

printf "[$PREFIX] Chrooting...\n"
read -r -d '' CHROOT_SCRIPT <<END_CHROOT

printf "[$PREFIX (chroot)] Installing basic packages...\n"
pacman -S $BASIC_PKG_LIST $HW_SPEC_PKG_LIST

printf "[$PREFIX (chroot)] Giving wheel sudo permissions...\n"
# visudo does lovely verification stuff for us so we'd like to use it, but
# the change we want to make is strictly programatic (uncommenting a line) so
# we use sed as the editor, passing in a script over stdin (-f- means script=stdin)
printf '%s' 's/^#\s*\(%wheel\s*ALL=(ALL.*)\s*ALL\)/\1/g' | EDITOR='sed -f- -i' visudo

printf "[$PREFIX (chroot)] Enable NetworkManager\n"
systemctl enable NetworkManager

printf "[$PREFIX (chroot)] Adding lvm2 hook to /etc/mkinitcpio.conf...\n"
sed -i '/^HOOKS=/s/block filesystems/block lvm2 filesystems/' /etc/mkinitcpio.conf
grep -n2 '^HOOKS=' /etc/mkinitcpio.conf
printf "[$PREFIX (chroot)] mkinitcpio\n"
mkinitcpio -p linux
mkinitcpio -p linux-lts

printf "[$PREFIX (chroot)] Selecting locale from /etc/locale.gen...\n"
sed -i 's/^#\s*\(en_US.UTF-8 UTF-8\)/\1/g' /etc/locale.gen
grep -n2 '^en' /etc/locale.gen
printf "[$PREFIX (chroot)] Generating locale...\n"
locale-gen

printf "[$PREFIX (chroot)] Creating users...\n"
printf "Set password for root user:\n"
passwd
useradd -m -g users -G video,wheel "$username"
printf "Set password for user $username:\n"
passwd "$username"
cd "/home/$username"

printf "[$PREFIX (chroot)] Setting up the bootloader!...\n"
mkdir -p /boot/EFI
fdisk -l
printf "Enter the EFI partition device name (i.e. the bit after /dev/) "
read efi_partition_device_name
mount /dev/\$efi_partition_device_name /boot/EFI
grub-install --target=x86_64-efi --bootloader-id=grub_uefi --recheck
mkdir -p /boot/grub/locale
cp /usr/share/locale/en\@quot/LC_MESSAGES/grub.mo /boot/grub/locale/en.mo
grub-mkconfig -o /boot/grub/grub.cfg
umount /boot/EFI

printf "[$PREFIX (chroot)] Setting up swap...\n"
dd if=/dev/zero of=/swapfile bs=1M count=2048 status=progress
chmod 600 /swapfile
mkswap /swapfile
printf '/swapfile none swap sw 0 0\n\n' >> /etc/fstab
cat /etc/fstab
mount -a
swapon -a
free -m

printf "[$PREFIX (chroot)] Setting the timezone to $timezone...\n"
ln -svf "/usr/share/zoneinfo/$timezone" /etc/localtime
hwclock --systohc

printf "[$PREFIX (chroot)] Setting the hostname to $hostname...\n"
printf '%s\n' "$hostname" > /etc/hostname
printf "[$PREFIX (chroot)] Setting hosts\n"
printf '127.0.0.1 localhost\n' >> /etc/hosts
printf '127.0.1.1 %s\n' "$hostname" >> /etc/hosts
cat /etc/hosts

printf "[$PREFIX (chroot)] Cloning repo $D_GIT_REMOTE_URL to ~$username/$D_GIT_TARGET_LOCAL_DIR...\n"
mkdir -p $D_GIT_TARGET_LOCAL_DIR
git clone $D_GIT_REMOTE_URL $D_GIT_TARGET_LOCAL_DIR

printf "[$PREFIX (chroot)] Installing pkglist from config repo...\n"
sed '/^#/d;/^\s*$/d;s/#.*$//g;s/\s*//g' $D_GIT_TARGET_LOCAL_DIR/pkglist | pacman -S -

printf "[$PREFIX (chroot)] Pushing config to system...\n"
BRI_SYNC_ROOT=/ BRI_SYNC_USER="$username" BRI_SYNC_REPO="$D_GIT_TARGET_LOCAL_DIR" "$D_GIT_TARGET_LOCAL_DIR/sync.sh" push

printf "[$PREFIX (chroot)] Symlinking font conf...\n"
mkdir -p .config/fontconfig/config.d
ln -sv /usr/share/fontconfig/config.avail .config/fontconfig/config.d/config.avail

# We grab a wallpaper from NASA because
# a) The alternative to some kind of wallpaper is inky blackness which isn't ideal
# b) NASA photos look really good
# c) NASA photos generally have really generous copyright requirements
# d) NASA has actually useable endpoints
printf "[$PREFIX (chroot)] Installing a default wallpaper...\n"
nasa_img='carina_nebula'
mkdir .wallpaper
curl -L \$(curl -s "https://images-api.nasa.gov/asset/\$nasa_img" | python3 -c "import sys, json; print(json.load(sys.stdin)['collection']['items'][0]['href'])") -o ".wallpaper/nasa-\$nasa_img"
ln -sv "nasa-\$nasa_img" .wallpaper/current

# Since we install as root everything that got placed into ~ belongs to root which causes problems
# E.g. pulse wants to create ~/.config/pulse but can't
# So as one of the final steps we chown everything to belong to the regular user
# Note that we do this after installing anything in ~ to ensure we don't miss anything,
# and that significantly, for cloning config into root we actually rely on the fact that the config
# files initially belong to root.
printf "[$PREFIX (chroot)] Chowning everything in ~$username/ to belong to the user...\n"
chown -R $username:users .

END_CHROOT
arch-chroot "$root_mnt" /bin/sh -c "$CHROOT_SCRIPT"
printf "[$PREFIX] Exiting chroot...\n"

printf "Installation finished! Check that everything's okay, then reboot.\n"

# Remaining tasks... (not automated yet)
# Secrets
# Setup network manager
# Setup firefox

## Phases
# 0a. Format partitions, setup hw, mount
# 0b. Nuke everything in / except move /home to /home/bak
# 1.  Pacstrap, chroot
# 2.  (in chroot) install and enable basics, config
# 3.  (in chroot) setup users
# 4.  (in chroot) grub
# 5.  (in chroot) setup swap, tz, hostname
# 6.  (in chroot) clone repo, propagate
# 7.  (in chroot) check, confirm, reboot
