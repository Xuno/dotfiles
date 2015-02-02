import vapoursynth as vs

core = vs.get_core()

clip = core.std.Trim(video_in, first=0, length=5000000)
video_out = core.bifrost.Bifrost(clip, interlaced=True)

video_out.set_output()
