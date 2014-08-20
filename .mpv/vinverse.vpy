import vapoursynth as vs

core = vs.get_core()

clip = video_in

if clip.format.bits_per_sample != 8:
    clip = core.fmtc.bitdepth(clip=c, bits=8)

video_out = core.vinverse.Vinverse(clip)

video_out.set_output()
