import vapoursynth as vs

core = vs.get_core()

# TODO: fix the chroma shift for subsampled footage.
double_height = lambda f: core.nnedi3.nnedi3(f, field=1, dh=True, planes=[0, 1, 2], nsize=4, nns=1)

video_out = core.std.Transpose(double_height(core.std.Transpose(double_height(video_in))))

video_out.set_output()
