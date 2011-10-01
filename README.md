Auto Ambient
============

Turn any piece of music into ambient music.

This function computes the spectrogram matrix of a signal, where each column corresponds to a frame and each row is an FFT bucket. Then a smoothing spline is applied for each row, horizonally, to create the ambient effect. Finally, the FFT is inversed and interpolated.

See the (well commented) source code for details.
