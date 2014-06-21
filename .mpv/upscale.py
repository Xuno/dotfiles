import vapoursynth as vs
core = vs.get_core()
w = video_in.width
h = video_in.height
#video_out = core.fmtcwrap.Scale(video_in, 2560, 1440, kernel="spline36")
video_out = core.fmtcwrap.Scale(video_in, w * 2, h * 2, kernel="spline36")
#video_out = core.fmtcwrap.Scale(video_in, w * 2, h * 2, kernel="spline64")
#video_out = core.std.Transpose(core.eedi3.eedi3(core.std.Transpose(core.eedi3.eedi3(video_in, field=1, dh=1)), field=1, dh=1))
video_out.set_output()
