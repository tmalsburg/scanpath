#!/usr/bin/env python
# -*- coding: utf-8 -*-

from math import sqrt, atan, acos, asin, tan, cos, sin, pi, atan2, fabs

def inverse_gnomonic(sc, center_x, center_y, distance, unit_size=1):

    res = []

    for x,y,d in sc:
        x = (x - center_x) * unit_size / distance
        y = (y - center_y) * unit_size / distance

        rho = sqrt(x**2 + y**2)
        c   = atan(rho)

        # FIXME At point 0,0 we get NaNs.  (Unlikely to happen with
        # real data.)

        lat   = asin(y * sin(c) / rho) * 180/pi
        lon   = atan2(x * sin(c), rho * cos(c)) * 180/pi

        res.append((lon, lat, d))

    return res

def which_min(*l):
    mi = 0
    for i,e in enumerate(l):
        if e<l[mi]:
            mi = i
    return mi

def scasim(s, t, center_x, center_y, distance, unit_size, modulator=0.83):

    if len(s) == 0:
        return t.duration(), None
    if len(t) == 0:
        return s.duration(), None

    s = inverse_gnomonic(s, center_x, center_y, distance, unit_size)
    t = inverse_gnomonic(t, center_x, center_y, distance, unit_size)

    # Prepare matrix:

    m, n = len(s), len(t)
    d = map(lambda i:[0]*(n+1), xrange(m+1))
    acc = 0
    for i in xrange(1,m+1):
        acc += s[i-1][2]
        d[i][0] = acc
    acc = 0
    for j in xrange(1,n+1):
        acc += t[j-1][2]
        d[0][j] = acc

    # Compute similarity:

    for i in xrange(n):
        for j in xrange(m):
            slon = s[j][0] / (180/pi)
            tlon = t[i][0] / (180/pi)
            slat = s[j][1] / (180/pi)
            tlat = t[i][1] / (180/pi)

            angle = acos(sin(slat) * sin(tlat) +
                    cos(slat) * cos(tlat) * cos(slon - tlon)) * (180/pi)

            mixer = modulator**angle

            cost = (abs(t[i][2] - s[j][2]) * mixer +
                       (t[i][2] + s[j][2]) * (1.0 - mixer))

            ops = (d[j][i+1] + s[j][2],
                   d[j+1][i] + t[i][2],
                   d[j][i]   + cost)

            mi = which_min(*ops)

            d[j+1][i+1] = ops[mi]

    return d[-1][-1]

if __name__ == '__main__':

    # Each tuple is a fixation with elements x-coordinate,
    # y-coordinate, duration:
    sc1 = [(100, 100, 100),
           (200, 200, 100),
           (300, 100, 100)]
    sc2 = [(100, 200, 100),
           (200, 300, 100),
           (300, 300, 100)]

    print scasim(sc1, sc2, 1024/2.0, 768/2.0, 60.0, 1/30.0, 0.8)

