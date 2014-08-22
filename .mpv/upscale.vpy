import vapoursynth as vs
import math

core = vs.get_core()

w, h = video_in_dw, video_in_dh
if w == 0 or h == 0:
    w, h = video.width, video_in.height
max_w, max_h = 2560, 1440

scale_ratio = min(max_w / w, max_h / h)

target_w = int(math.ceil(w * scale_ratio / 2.0 - 0.1)) * 2
target_h = int(math.ceil(h * scale_ratio / 2.0 - 0.1)) * 2

clip = video_in

if clip.width == 720 and clip.height == 480 and (w != clip.width or h != clip.height):
    clip = core.std.CropRel(clip, left=8, right=8)

upscaled = core.fmtc.resample(clip, target_w, target_h, kernel="spline", taps=6, csp=vs.YUV444P16)
upscaled_nr = core.fmtc.resample(clip, target_w, target_h, kernel="gauss", a1=100, csp=vs.YUV444P16)

video_out = core.rgvs.Repair(upscaled, upscaled_nr, 1)

video_out.set_output()
