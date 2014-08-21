import vapoursynth as vs
import functools

def pp_select(n, f, origin, decomb):
    return decomb if f.props._Combed else origin

def print_combed(n, f, origin, decomb):
    # dont forget to set micout=True
    combed = "*COMBED*" if f.props._Combed else "clean"
    mic = str(f.props.VFMMics[f.props.VFMMatch])
    sc = "SC" if f.props.VFMSceneChange else ""
    output = decomb if f.props._Combed else origin
    return core.text.Text(output, combed + " " + mic + " " + sc)

core = vs.get_core()

clip = core.std.Trim(video_in, first=0, length=5000000)

if clip.format != vs.YUV420P8:
    clip = core.resize.Bicubic(clip, format=vs.YUV420P8)

# TODO: control order
fmed = core.vivtc.VFM(clip, order=1, blockx=64, cthresh=8, mi=50, micout=False)
cleaned = core.vinverse.Vinverse(fmed)

pped = core.std.FrameEval(fmed, functools.partial(pp_select, origin=fmed, decomb=cleaned), prop_src=fmed)

comb_info = core.std.FrameEval(fmed, functools.partial(print_combed, origin=fmed, decomb=cleaned), prop_src=fmed)

video_out = pped

video_out.set_output()
