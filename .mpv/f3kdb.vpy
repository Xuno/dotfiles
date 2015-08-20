import vapoursynth as vs

core = vs.get_core()

clip = video_in
video_out = core.f3kdb.Deband(clip, grainy=0, grainc=0, output_depth=16)

video_out.set_output()
