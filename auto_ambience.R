## Auto Ambience
## Copyright (C) 2011  Andreas Jansson <andreas@jansson.me.uk>

## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or (at
## your option) any later version.

## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.


##
## Turn any piece of music into ambient music. The DSP equivalent
## of DJ Screw.
## 
## Example usage (requires tuneR):
##   input <- readMP3("claire_de_lune.mp3")
##   input <- downsample(input, 11025)
##   input <- mono(input)
##   output <- ambify(input@left)
##   output <- Wave(output[[1]], output[[2]], bit=16, samp.rate=11025)
##   output <- normalize(output, "16")
##   writeWave(output, "claire_de_lune_ambient.wav")
##   
auto.ambience <- function(audio, window.size = 4096, threshold = .996, smooth = .3,
                          max.deviation = 1) {

  # calculate spectrogram as a n*m matrix where the fft is vertical
  # and the frames are horizontal, i.e. it will have window.size rows
  # and length(audio) / window.size (== number of frames) columns
  spec <- matrix(0, ncol = floor(length(audio) / window.size) + 1, nrow=window.size)
  hanning.window <- .5 - .5 * cos(2 * pi * 1:window.size / window.size)
  for(i in 1:floor(length(audio) / window.size)) {
    spec[,i] <- fft(audio[(window.size * (i - 1) + 1):(window.size * i)] * hanning.window)
  }

  # very primitive peak detection by zeroing out all spectral components
  # below a given quantile threshold (usually set very high, e.g. 99.6%)
  spec[Mod(spec) < quantile(Mod(spec), threshold)] <- 0

  # "compress" the amplitudes
  spec <- sqrt(Mod(spec))

  for(i in 1:window.size) {
    # smooth the spectrogram horizontally, i.e. over time. by now we
    # completely disregard phase, we even set the imaginary part to 0.
    spec[i, ] <- smooth.spline(Mod(spec[i,]), spar = smooth)$y
  }
  # clean up after smooth function by turning any negative components
  # to 0.
  spec[Mod(spec) < 0] <- 0

  # initialise stereo channels
  left <- integer(length(audio))
  right <- integer(length(audio))

  # randomly deviate from inteded frequency to avoid stacking overtones of
  # window.size, which creates a beat at the frequency of window.size
  deviations <- runif(window.size / 2 - 1, -max.deviation, max.deviation)

  # manually compute the ifft. we do this to allow us to interpolate between
  # amplitudes, thus avoiding clicks at frame boundaries.
  for(x in 1:(ncol(spec) - 1)) {
    for(y in 1:(window.size / 2 - 1)) {
      cur <- Mod(spec[y, x])
      nxt <- Mod(spec[y, x + 1])

      if(cur > 0 || nxt > 0) {
        frames <- (window.size * (x - 1) + 1):(window.size * x)

        # compute the waveform, taking into account frequency deviation
        # and the resulting phase shift and linear interpolation
        phase.shift <- 2 * pi * (deviations[y] - 1) * x
        frequency <- y + deviations[y]
        interpolation <- seq(cur, nxt, length.out = window.size)
        wave <- cos(phase.shift + (1:window.size * frequency * 2 * pi / window.size)) *
          interpolation

        # *harsh* stereo spread
        if(y %% 2 == 0)
          left[frames] <- left[frames] + wave
        else
          right[frames] <- right[frames] + wave
      }
    }
  }

  return(list(left, right))
}

