import sys
import matplotlib
import numpy as np
import matplotlib.pyplot as plt

def plot_csv(title, ylabel, outname, fn, separate_figures = False):
    rows = [l.strip().split('\t') for l in open(fn).readlines()]

    names = rows[0]
    other_names = names[1:]

    data_rows = map(lambda r: map(float, r), rows[1:])

    max_row_len = max(map(len, data_rows))

    pad_row = lambda r: r + [None] * (max_row_len - len(r))

    data_rows = map(pad_row, data_rows)

    plt.xlabel(names[0])

    plt.ylabel(ylabel)

    data_cols = map(lambda c: filter(lambda x: x != None, c), zip(*data_rows))

    xcol = data_cols[0]

    all_plt_inputs = map(lambda (n, c): (zip(*zip(xcol, c)), n), zip(other_names, data_cols[1:]))

    counter = 1
    for ((x, y), name) in all_plt_inputs:
        if separate_figures:
            plt.subplot(len(all_plt_inputs), 1, counter)
            plt.plot(x, y, '-o', label = name)
            plt.legend(loc = 'upper right')
            counter += 1
        else:
            plt.plot(x, y, '-o', label = name)

    if not separate_figures:
        plt.legend(loc = 'lower left')
    plt.suptitle(title)
    plt.savefig(outname)
    plt.clf()

def test():
    plot_csv('concept1.csv', 'Log likelihood', 'concept1.pdf', 'concept1.csv')
