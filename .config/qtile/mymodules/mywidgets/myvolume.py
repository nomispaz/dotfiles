import asyncio
import re
import subprocess

from libqtile import bar
from libqtile.command.base import expose_command
from libqtile.log_utils import logger
from libqtile.utils import acall_process, create_task
from libqtile.widget import base

__all__ = [
    "Volume",
]

class VolumeBase(base._TextBox):
    orientations = base.ORIENTATION_HORIZONTAL
    defaults = [
        ("padding", 3, "Padding left and right. Calculated if None."),
        ("mute_foreground", None, "Foreground color for mute volume."),
        ("mute_format", "\uf6a9", "Format to display when volume is muted."),
        ("unmute_format", " {volume}%", "Format of text to display when volume is not muted."),
    ]

    def __init__(self, **config):
        base._TextBox.__init__(self, "", **config)
        self.add_defaults(VolumeBase.defaults)
        self.surfaces = {}
        self.volume = None
        self.is_mute = False

    def _configure(self, qtile, parent_bar):
        base._TextBox._configure(self, qtile, parent_bar)
        self.unmute_foreground = self.foreground

    def _update_drawer(self):
        if self.mute_foreground is not None:
            self.layout.colour = self.mute_foreground if self.is_mute else self.unmute_foreground

        self.text = (
            self.mute_format if self.is_mute or self.volume < 0 else self.unmute_format
        ).format(volume=self.volume)

    def draw(self):
        base._TextBox.draw(self)

class Volume(VolumeBase):
    """Widget that display and change volume

    By default, this widget uses wpctl to get and set the volume so users
    will need to make sure this is installed.
    """

    defaults = [
        ("update_interval", 0.5, "Update time in seconds."),
        ("mute_command", "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle", "Mute command"),
        ("volume_app", "pavucontrol", "App to control volume"),
        ("volume_up_command", "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+", "Volume up command"),
        ("volume_down_command", "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-", "Volume down command"),
        (
            "get_volume_command",
            "wpctl get-volume @DEFAULT_AUDIO_SINK@",
            "Command to get the current volume. "
            "The expected output should include x.yz",
        ),
        ("check_mute_string", "MUTED", "String expected to be included from check_mute_command when volume is muted."
        ),
    ]

    def __init__(self, **config):
        VolumeBase.__init__(self, **config)
        self.add_defaults(Volume.defaults)

        self.add_callbacks(
            {
                "Button1": self.mute,
                "Button3": self.run_app,
                "Button4": self.increase_vol,
                "Button5": self.decrease_vol,
            }
        )

    def timer_setup(self):
        create_task(self.do_volume())
        if self.theme_path:
            self.setup_images()

    def button_press(self, x, y, button):
        base._TextBox.button_press(self, x, y, button)
        self.draw()

    async def do_volume(self):
        vol, muted = await self.get_volume()
        if vol != self.volume or muted != self.is_mute:
            self.volume = vol
            self.is_mute = muted
            # Update the underlying canvas size before actually attempting
            # to figure out how big it is and draw it.
            self._update_drawer()
            self.bar.draw()
        await asyncio.sleep(self.update_interval)
        create_task(self.do_volume())

    async def get_volume(self):
        try:
            get_volume_cmd = self.get_volume_command
            mixer_out = await acall_process(get_volume_cmd, shell=True)
        except subprocess.CalledProcessError:
            return -1, False

        """
        Parse wpctl volume output.
        Expects strings like:
          'Volume: 0.35'
          'Volume: 0.35 [MUTED]'
        Returns:
          (volume_percent: int, muted: bool)
        """

        # Extract the volume as a float
        match = re.search(r"Volume:\s*([0-9.]+)", mixer_out)
        if match:
            vol_float = float(match.group(1))       # 0.35
            vol_percent = int(vol_float * 100)      # convert to %
        else:
            vol_percent = 0

        # Check if muted
        muted = "[MUTED]" in mixer_out
        
        if vol_percent:
            return vol_percent, muted
        else:
            # this shouldn't happen
            return -1, muted

    @expose_command()
    def increase_vol(self):
        volume_up_cmd = self.volume_up_command
       
        subprocess.call(volume_up_cmd, shell=True)

    @expose_command()
    def decrease_vol(self):
        volume_down_cmd = self.volume_down_command
        
        subprocess.call(volume_down_cmd, shell=True)

    @expose_command()
    def mute(self):
        mute_cmd = self.mute_command
        
        subprocess.call(mute_cmd, shell=True)

    @expose_command()
    def run_app(self):
        if self.volume_app is not None:
            subprocess.Popen(self.volume_app, shell=True)
