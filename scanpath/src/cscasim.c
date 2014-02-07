#include <stdlib.h>
#include <stdio.h>
#define _USE_MATH_DEFINES
#include <math.h>

void print_matrix(double** d, int n, int m)
{
    int i,j;

    for (i=0; i < n; i++) {
        for (j=0; j < m; j++) {
            printf("%.1f ", d[i][j]);
        }
        printf("\n");
    }
}

/* NOTE: Is inlining equivalent to making this a macro?  Check out
 * always_inline (non-standard GCC feature).  According to the GCC manual it's
 * almost as fast as a macro (whatever that means). */
inline double fmin3(double a, double b, double c)
{
    double t;
    t = (a <= b || isnan (b)) ? a : b;
    t = (t <= c || isnan (c)) ? t : c;
    return t;
}

double** create_matrix(int* ns, double* sd,
                       int* nt, double* td)
{

    double** d;
    double accumulator;
    int i, j;
    int m, n;
    n = *ns;
    m = *nt;

    d = malloc(sizeof(double*) * (n + 1));
    for (i=0; i <= n; i++)
        d[i] = malloc(sizeof(double) * (m + 1));

    /* initialize matrix */

    d[0][0] = 0.0;
    accumulator = 0.0;
    for (i=0; i < n; i++) {
        accumulator += sd[i];
        d[i+1][0] = accumulator;
    }
    accumulator = 0.0;
    for (j=0; j < m; j++) {
        accumulator += td[j];
        d[0][j+1] = accumulator;
    }

    return d;
}

void free_matrix(double** d, int* ns) {

    int i;

    for (i=0; i <= *ns; i++)
        free(d[i]);
    free(d);

}

/* 
 * Computes the similarity of two scanpaths.  Fixation sites are defined using
 * x and y coordinates.  ns is the number of fixations, sx is an array with the
 * x coordinates of the fixations (length *ns), sy are the y-coordinates, sd
 * the fixation durations (usually in milliseconds)
 * */
void cscasim(int* ns, double* slon, double* slat, double* sd,
             int* nt, double* tlon, double* tlat, double* td,
             double* modulator,
             double* result)
{
    double** d;
    double angle, mixer;
    double sa, sb, ta, tb;
    double cost;
    int i, j;
    int m, n;
    n = *ns;
    m = *nt;

    /* allocate memory for 2-d matrix: */
    d = create_matrix(ns, sd, nt, td);

    /* calculate scanpath dissimilarity: */

    /* loop over fixations in scanpath s: */
    for (i=0; i < n; i++) {
        /* loop over fixations in scanpath t: */
        for (j=0; j < m; j++) {

            /* calculating angle between fixation targets: */
            sa = slon[i] / (180/M_PI);
            ta = tlon[j] / (180/M_PI);
            sb = slat[i] / (180/M_PI);
            tb = tlat[j] / (180/M_PI);

            /* This formula is not terribly precise but the error is way
             * smaller than the spatial noise in current eye trackers. */
            angle = acos(sin(sb) * sin(tb) +
                    cos(sb) * cos(tb) * cos(sa - ta)) * (180/M_PI);

            /* approximation of cortical magnification: */
            mixer = pow(*modulator, angle);

            /* cost for substitution: */
            cost = fabs(td[j] - sd[i]) * mixer +
                   (td[j] + sd[i])     * (1.0 - mixer);

            /* select optimal edit operation: */
            d[i+1][j+1] = fmin3(d[i][j+1] + sd[i],
                                d[i+1][j] + td[j],
                                d[i][j]   + cost);
        }
    }

    *result = d[n][m];

    /* free memory: */

    free_matrix(d, ns);

}


/* vim:ts=4:sw=4
 * */
