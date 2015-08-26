# -*- coding: utf-8 -*-

# This is an example module to be used as a template.
# See https://github.com/ultrabug/py3status/wiki/Write-your-own-modules
# for more details.
#
# NOTE: py3status will NOT execute:
#     - methods starting with '_'
#     - methods decorated by @property and @staticmethod
#
# NOTE: reserved method names:
#     - 'kill' method for py3status exit notification
#     - 'on_click' method for click events from i3bar (read below please)
#
# WARNING:
#
# Do NOT use print on your modules: py3status will catch any output and discard
# it silently because this would break your i3bar (see issue #20 for details).
# Make sure you catch any output from any external program you may call
# from your module. Any output from an external program cannot be caught and
# silenced by py3status and will break your i3bar so please, redirect any
# stdout/stderr to /dev/null for example (see issue #20 for details).
#
# CONTRIBUTORS:
#
# Contributors are kindly requested to agree to contribute their code under
# the BSD license to match py3status' one.
#
# Any contributor to this module should add his/her name to the @author
# line, comma separated.
#
# DOCSTRING:
#
# Fill in the following docstring: it will be parsed by py3status to document
# your module from the CLI.
"""
One-line summary followed by an empty line.
Multi-line description followed by an empty line.
Configuration parameters:
    - cache_timeout : how often we refresh this module in seconds
@author <your full name> <your email address>
@license BSD
"""

from datetime import timedelta
from time import time
from time import sleep
from gi.repository import Playerctl, GLib
import thread

class Py3status:
    """
    The Py3status class name is mandatory.
    Below you list all the available configuration parameters and their
    default value for your module which can be overwritten by users
    directly from their i3status config.
    This examples features only one parameter which is 'cache_timeout'
    and is set to 10 seconds (0 would mean no cache).
    """
    # available configuration parameters
    cache_timeout = 5
    format = u'{artist} : {title}'
    color_pause = '#999900'
    color_play = '#BBFF00'

    # internal variables
    _song_playing = False
    _title = 'Title'
    _artist = 'Artist'
    _album = 'Album'
    _rtime = '0'
    _color = None

    def _setup_player(self):
        player = Playerctl.Player()
        self._player = player
        player_name = player.get_property('player-name')
        if player_name == None:
            self._song_playing = False
            self.cache_timeout = 5
        else:
            self.cache_timeout = 1
            player.on('metadata', self._on_metadata)
            player.on('stop', self._on_stop)
            player.on('play', self._on_play)
            player.on('pause', self._on_pause)
            self._update_metadata(player.get_property('metadata'))
            self._update_status()
        pass

    def _on_metadata(self, player, data):
        self._update_metadata(data)
        pass

    def _on_stop(self, player):
        self._update_status()
        pass

    def _on_play(self, player):
        self._update_status()
        pass

    def _on_pause(self, player):
        self._update_status()
        pass

    def _update_status(self):
        status = self._player.get_property('status')
        if status == 'Playing':
            self._song_playing = True
            self._color = self.color_play
        elif status == 'Paused':
            self._song_playing = True
            self._color = self.color_pause
        elif status == 'Stopped':
            self._song_playing = False
        pass

    def _update_metadata(self, data):
        if not data:
            self._title = 'None'
            self._artist = 'None'
            self._album = 'None'
            self._rtime = '0'
        else:
            self._title = data['xesam:title']
            self._artist = '/'.join(data['xesam:artist'])
            self._album = data['xesam:album']
            self._rtime = str(timedelta(microseconds=data['mpris:length']))
        pass

    def _update_text(self):
        if self._song_playing == False:
            self.full_text = 'Nothing'
        else:
            self.full_text = self.format.format(title=self._title,
                                                artist=self._artist,
                                                album=self._album,
                                                time=self._rtime)
        pass

    def __init__(self):
        """
        This is the class constructor which will be executed once.
        """
        loop = GLib.MainLoop()
        self._loop = loop.get_context()
        self._setup_player()
        pass

    def media(self, i3s_output_list, i3s_config):
        """
        This method will return an empty text message
        so it will NOT be displayed on your i3bar.
        If you want something displayed you should write something
        in the 'full_text' key of your response.
        See the i3bar protocol spec for more information:
        http://i3wm.org/docs/i3bar-protocol.html
        """
        """
        if i3s_config['format']:
            self.format = i3s_config['format']
        """

        if self._song_playing == False:
            self._setup_player()
        else:
            if self._loop.iteration(False):
                self._update_text()

        response = {
            'cached_until': time() + self.cache_timeout,
            'full_text': self.full_text,
            'color' : self._color,
            'name': 'media'
        }
        return response

if __name__ == "__main__":
    """
    Test this module by calling it directly.
    This SHOULD work before contributing your module please.
    """
    x = Py3status()
    config = {
        'format' : '{title} - {album} ({artist}) {time}'
    }
    while True:
        print(x.media([], config))
        sleep(1)
