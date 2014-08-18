import vapoursynth as vs
import math

core = vs.get_core()

w, h = video_in.width, video_in.height
max_w, max_h = 2560, 1440
my_kernel = "spline64"

scale_ratio = min(max_w / w, max_h / h)

target_w = int(math.ceil(w * scale_ratio - 0.1))
target_h = int(math.ceil(h * scale_ratio - 0.1))

video_out = core.fmtc.resample(video_in, target_w, target_h, kernel=my_kernel, csp=vs.YUV444P16)

video_out.set_output()
