import vapoursynth as vs

core = vs.get_core()

clip = core.std.Trim(video_in, first=0, length=5000000)
separated = core.std.SeparateFields(clip, tff=True)
even = core.std.SelectEvery(separated, 2, 0)

even.set_output()
