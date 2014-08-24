import vapoursynth as vs

core = vs.get_core()

clip = video_in

if clip.format.bits_per_sample > 8:
    clip = core.fmtc.bitdepth(clip, bits=8, flt=0, dmode=5)

clip.set_output()
