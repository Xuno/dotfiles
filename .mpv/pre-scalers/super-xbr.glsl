#define XBR_EDGE_STR 1.0
#define XBR_WEIGHT 1.0

/*

    *******  Super XBR Shader  *******

    Copyright (c) 2015 Hyllian - sergiogdb@gmail.com

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.

*/

#if pass == 0
    #define wp1  2.0
    #define wp2  1.0
    #define wp3 -1.0
    #define wp4  4.0
    #define wp5 -1.0
    #define wp6  1.0

    #define weight1 (XBR_WEIGHT*1.29633/10.0)
    #define weight2 (XBR_WEIGHT*1.75068/10.0/2.0)
#elif pass == 1
    #define wp1  2.0
    #define wp2  0.0
    #define wp3  0.0
    #define wp4  0.0
    #define wp5  0.0
    #define wp6  0.0

    #define weight1 (XBR_WEIGHT*1.75068/10.0)
    #define weight2 (XBR_WEIGHT*1.29633/10.0/2.0)
#endif

float df(float A, float B)
{
    return abs(A-B);
}

float d_wd(float b0, float b1, float c0, float c1, float c2, float d0, float d1, float d2, float d3, float e1, float e2, float e3, float f2, float f3)
{
    return (wp1*(df(c1,c2) + df(c1,c0) + df(e2,e1) + df(e2,e3)) + wp2*(df(d2,d3) + df(d0,d1)) + wp3*(df(d1,d3) + df(d0,d2)) + wp4*df(d1,d2) + wp5*(df(c0,c2) + df(e1,e3)) + wp6*(df(b0,b1) + df(f2,f3)));
}

float hv_wd(float i1, float i2, float i3, float i4, float e1, float e2, float e3, float e4)
{
    return ( wp4*(df(i1,i2)+df(i3,i4)) + wp1*(df(i1,e1)+df(i2,e2)+df(i3,e3)+df(i4,e4)) + wp3*(df(i1,e2)+df(i3,e4)+df(e1,i2)+df(e3,i4)));
}

float min4(float a, float b, float c, float d)
{
    return min(min(a, b), min(c, d));
}

float max4(float a, float b, float c, float d)
{
    return max(max(a, b), max(c, d));
}

float sample(sampler2D tex, int plane, vec2 pos, vec2 tex_size)
{
    //Skip pixels on wrong grid
#if pass == 0
    vec2 dir = fract(pos * tex_size) - 0.5;
    if (dir.x < 0 || dir.y < 0) return texture(tex, pos - dir / tex_size)[plane];
    #define Get(x, y) (texture(tex, pos + (vec2(x, y) - vec2(0.25, 0.25)) / tex_size)[plane])
#elif pass == 1
    vec2 dir = fract(pos * tex_size / 2) - 0.5;
    if (dir.x * dir.y > 0) return texture(tex, pos)[plane];
    #define Get(x, y) (texture(tex, pos + (vec2((x) + (y) - 1, (y) - (x))) / tex_size)[plane])
#endif

    float P0 = Get(-1,-1);
    float P1 = Get( 2,-1);
    float P2 = Get(-1, 2);
    float P3 = Get( 2, 2);

    float  B = Get( 0,-1);
    float  C = Get( 1,-1);
    float  D = Get(-1, 0);
    float  E = Get( 0, 0);
    float  F = Get( 1, 0);
    float  G = Get(-1, 1);
    float  H = Get( 0, 1);
    float  I = Get( 1, 1);

    float F4 = Get(2, 0);
    float I4 = Get(2, 1);
    float H5 = Get(0, 2);
    float I5 = Get(1, 2);

/*
                                  P1
         |P0|B |C |P1|         C     F4          |a0|b1|c2|d3|
         |D |E |F |F4|      B     F     I4       |b0|c1|d2|e3|   |e1|i1|i2|e2|
         |G |H |I |I4|   P0    E  A  I     P3    |c0|d1|e2|f3|   |e3|i3|i4|e4|
         |P2|H5|I5|P3|      D     H     I5       |d0|e1|f2|g3|
                               G     H5
                                  P2
*/

    /* Calc edgeness in diagonal directions. */
    float d_edge  = (d_wd( D, B, G, E, C, P2, H, F, P1, H5, I, F4, I5, I4 ) - d_wd( C, F4, B, F, I4, P0, E, I, P3, D, H, I5, G, H5 ));

    /* Calc edgeness in horizontal/vertical directions. */
    float hv_edge = (hv_wd(F, I, E, H, C, I5, B, H5) - hv_wd(E, F, H, I, D, F4, G, I4));

    /* Filter weights. Two taps only. */
    vec4 w1 = vec4(-weight1, weight1+0.5, weight1+0.5, -weight1);
    vec4 w2 = vec4(-weight2, weight2+0.25, weight2+0.25, -weight2);

    /* Filtering and normalization in four direction generating four colors. */
    float c1 = dot(vec4(P2, H, F, P1), w1);
    float c2 = dot(vec4(P0, E, I, P3), w1);
    float c3 = dot(vec4( D+G, E+H, F+I, F4+I4), w2);
    float c4 = dot(vec4( C+B, F+E, I+H, I5+H5), w2);

    float limits = XBR_EDGE_STR + 0.000001;
    float edge_strength = smoothstep(0.0, limits, abs(d_edge));

    /* Smoothly blends the two strongest directions (one in diagonal and the other in vert/horiz direction). */
    float color =  mix(mix(c1, c2, step(0.0, d_edge)), mix(c3, c4, step(0.0, hv_edge)), 1 - edge_strength);

    /* Anti-ringing code. */
    float min_sample = min4(  E,   F,   H,   I);
    float max_sample = max4(  E,   F,   H,   I);
    float aux = color;
    color = clamp(color, min_sample, max_sample);
    color = mix(aux, color, 1-2.0*abs(edge_strength-0.5));

    return color;
}
