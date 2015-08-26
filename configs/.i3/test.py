# -*- coding: utf-8 -*-

from datetime import timedelta
from time import time
from gi.repository import Playerctl, GLib
import thread

player = Playerctl.Player()

def on_metadata(player, e):
    if 'xesam:artist' in e.keys() and 'xesam:title' in e.keys():
        print('Now playing:')
        print('{artist} - {title}'.format(artist=e['xesam:artist'][0], title=e['xesam:title']))

def on_play(player):
    print('Playing at volume {}'.format(player.props.volume))

def on_pause(player):
    print('Paused the song: {}'.format(player.get_title()))

def on_stop(player):
    print('Stopped');

player.on('metadata', on_metadata)
player.on('stop', on_stop)
player.on('play', on_play)
player.on('pause', on_pause)

from time import sleep

# wait for events
GLib.threads_init()
main = GLib.MainLoop().get_context()
while (1):
    main.iteration(False)
    sleep(0.5)
