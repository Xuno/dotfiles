import vapoursynth as vs

core = vs.get_core()

video_out = core.nnedi3.nnedi3_rpow2(video_in, rfactor=2, correct_shift=False)

video_out.set_output()
