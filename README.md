This is an example of how to use Haskell to talk to an
[Open Pixel Control][1] server, such as a [FadeCandy][2], to control
strings of RGB LED lights.

Additionally, this program listens for a button press on GPIO2, and
switches between different patterns of lights each time the button is
pressed.

The button should be connected between GPIO2 and GND, which are pins
13 and 14 on the Raspberry Pi's main GPIO connector.

This program uses the [wiringPi][4] C library to talk to the GPIO, so
you'll need to install wiringPi before building this program.  On
Raspbian "Jessie", wiringPi is available as a package:

    sudo apt-get install wiringpi

You'll also need to set the `WIRINGPI_GPIOMEM` environment variable
(to any value) before running this program, so that you don't have to
run it as root.

For more information, see [my blog post][3].

[1]: http://openpixelcontrol.org/
[2]: https://github.com/scanlime/fadecandy/
[3]: http://funwithsoftware.org/posts/2016-12-26-gpio-on-raspberry-pi-with-haskell.html
[4]: http://wiringpi.com/
