# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/34a567a8-a222-4658-b5b1-3f30d89d6562";
      fsType = "btrfs";
      options = [ "subvol=root" ];
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/34a567a8-a222-4658-b5b1-3f30d89d6562";
      fsType = "btrfs";
      options = [ "subvol=home" ];
    };

  fileSystems."/nix" =
    { device = "/dev/disk/by-uuid/34a567a8-a222-4658-b5b1-3f30d89d6562";
      fsType = "btrfs";
      options = [ "subvol=nix" ];
    };

  fileSystems."/.snapshots" =
    { device = "/dev/disk/by-uuid/34a567a8-a222-4658-b5b1-3f30d89d6562";
      fsType = "btrfs";
      options = [ "subvol=snapshots" ];
    };

  fileSystems."/var/log" =
    { device = "/dev/disk/by-uuid/34a567a8-a222-4658-b5b1-3f30d89d6562";
      fsType = "btrfs";
      options = [ "subvol=var_log" ];
    };

  fileSystems."/swap" =
    { device = "/dev/disk/by-uuid/34a567a8-a222-4658-b5b1-3f30d89d6562";
      fsType = "btrfs";
      options = [ "subvol=swap" ];
    };

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/AD83-1DD5";
      fsType = "vfat";
      options = [ "fmask=0022" "dmask=0022" ];
    };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.eno1.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp4s0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
