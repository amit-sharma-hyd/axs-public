from __future__ import print_function
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

def barPlotY(y, title="", xaxis="", yaxis=""): 
    data = [go.Bar(x=y.index.values, y=y)]
    layout = go.Layout(title=title,
                    xaxis=dict(title=xaxis),
                    yaxis=dict(title=yaxis))
    fig = go.Figure(data=data, layout=layout)
    iplot(fig, show_link=False)

def barPlotXY(x, y, title="", xaxis="", yaxis=""): 
    data = [go.Bar(x=x, y=y)]
    layout = go.Layout(title=title,
                    xaxis=dict(title=xaxis),
                    yaxis=dict(title=yaxis))
    fig = go.Figure(data=data, layout=layout)
    iplot(fig, show_link=False)

def stackedBarPlotY(y, title="", xaxis="", yaxis=""): 
    data = []
    for i in len(y):
        data.append(go.Bar(x=y[i].index.values, y=y[i]))
    layout = go.Layout(title=title,
                    xaxis=dict(title=xaxis),
                    yaxis=dict(title=yaxis))
    fig = go.Figure(data=data, layout=layout)
    iplot(fig, show_link=False)

def groupedBoxPlot(df, col=None, byCol=None, plotinline=True, title="", xaxis="", yaxis=""): 
    if byCol == None:
        col = byCol
    data = []
    for byColVal in df[byCol].unique():
        data.append(go.Box(y=df[df[byCol]==byColVal][col]))
    layout = go.Layout(title=title,
                    xaxis=dict(title=xaxis),
                    yaxis=dict(title=yaxis))
    fig = go.Figure(data=data, layout=layout)
    if (plotinline):
        iplot(fig, show_link=False)
    else:
        return fig    

def print_cm(cm, labels, hide_zeroes=False, hide_diagonal=False, hide_threshold=None):
    """pretty print for confusion matrixes"""
    columnwidth = max([len(x) for x in labels] + [5])  # 5 is value length
    empty_cell = " " * columnwidth
    
    # Begin CHANGES
    fst_empty_cell = (columnwidth-3)//2 * " " + "t/p" + (columnwidth-3)//2 * " "
    
    if len(fst_empty_cell) < len(empty_cell):
        fst_empty_cell = " " * (len(empty_cell) - len(fst_empty_cell)) + fst_empty_cell
    # Print header
    print("    " + fst_empty_cell, end=" ")
    # End CHANGES
    
    for label in labels:
        print("%{0}s".format(columnwidth) % label, end=" ")
        
    print()
    # Print rows
    for i, label1 in enumerate(labels):
        print("    %{0}s".format(columnwidth) % label1, end=" ")
        for j in range(len(labels)):
            cell = "%{0}.1f".format(columnwidth) % cm[i, j]
            if hide_zeroes:
                cell = cell if float(cm[i, j]) != 0 else empty_cell
            if hide_diagonal:
                cell = cell if i != j else empty_cell
            if hide_threshold:
                cell = cell if cm[i, j] > hide_threshold else empty_cell
            print(cell, end=" ")
        print()

def print_confusion_matrix(confusion_matrix, class_names, figsize = (6,5), fontsize=12):
    """Prints a confusion matrix, as returned by sklearn.metrics.confusion_matrix, as a heatmap.
    
    Arguments
    ---------
    confusion_matrix: numpy.ndarray
        The numpy.ndarray object returned from a call to sklearn.metrics.confusion_matrix. 
        Similarly constructed ndarrays can also be used.
    class_names: list
        An ordered list of class names, in the order they index the given confusion matrix.
    figsize: tuple
        A 2-long tuple, the first value determining the horizontal size of the ouputted figure,
        the second determining the vertical size. Defaults to (10,7).
    fontsize: int
        Font size for axes labels. Defaults to 14.
        
    Returns
    -------
    matplotlib.figure.Figure
        The resulting confusion matrix figure
    """
    df_cm = pd.DataFrame(
        confusion_matrix, index=class_names, columns=class_names, 
    )
    fig = plt.figure(figsize=figsize)
    try:
        heatmap = sns.heatmap(df_cm, annot=True, fmt="d")
    except ValueError:
        raise ValueError("Confusion matrix values must be integers.")
    heatmap.yaxis.set_ticklabels(heatmap.yaxis.get_ticklabels(), rotation=0, ha='right', fontsize=fontsize)
    heatmap.xaxis.set_ticklabels(heatmap.xaxis.get_ticklabels(), rotation=0, ha='right', fontsize=fontsize)
    plt.ylabel('True label')
    plt.xlabel('Predicted label')
    return fig
