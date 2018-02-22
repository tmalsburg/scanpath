#!/usr/bin/env python
# -*- coding: utf-8 -*-

import pdb
debug = False
def toggle_debug():
    global debug
    debug = not debug

import sys, os.path
import Tkinter
import Tkdnd
import tkFileDialog
from Tkconstants import PAGES, UNITS, NORMAL, RAISED, SUNKEN, HORIZONTAL, RIGHT, BOTH, LEFT, BOTTOM, TOP, NW, HIDDEN, X, Y, ALL, PROJECTING, MITER, ROUND, LAST, RIDGE, DISABLED, NORMAL, W
from math import sqrt, atan, acos, asin, tan, cos, sin, pi, atan2, fabs
from numpy import array, array2string


# TODO Add a menu item that opens a side panel containing config
#      options for scasim (viewing distance) and display (grid, axis).
# TODO Add a revert button, that rereads the current file.
# TODO Fixations shouldn't overlap scanpath lines.
# TODO Use different colors for the two scanpaths.
# TODO Allow switching the display to inverse.gnomonic projection?
#      Better have this as a separate viewer.  Select a fixation and
#      the window shows the "cortical" projection with this fixation
#      in the center.
# TODO Add a selector for sample stimuli: a sentence, a picture, nothing.
#      (The horse raced ...)
# TODO Add a target showing the center of the visual field.
# TODO Logic related to Scasim should be separated from the logic of
#      the meter.
# TODO Drag&drop on fixation circles moves fixation.  Drag&drop on
#      line moves whole scanpath (include some halo around lines).

# DONE Read scanpaths from file specified on command line.
# DONE Also display other measures such as Levenshtein, Hamming,
#      Euclidean.
# DONE Use scrollwheel to set fixation durations.

#
# View:
#

class MainFrame(Tkinter.Frame):
    def __init__(self, master, controller, **opts):
        Tkinter.Frame.__init__(self, master, **opts)
        master.title("Scanpath Edit")
        self.controller = controller
        # Menubar:
        self.menu = Tkinter.Menu(master)
        self.open_dialog = tkFileDialog.Open(master)
        self.save_dialog = tkFileDialog.SaveAs(master)
        # self.menu.add_command(label="open",
        #     command=lambda:controller.open_file(self.open_dialog.show()))
        self.menu.add_command(label="save",
            command=lambda:controller.write_file(), state=DISABLED)
        self.menu.add_command(label="save as",
            command=lambda:controller.write_file(self.save_dialog.show()))
        #self.menu.add_command(label="reset", command=lambda:1)
        self.menu.add_command(label="undo",
                              command=lambda:controller.undo_manager.undo(),
                              state=DISABLED)
        self.menu.add_command(label="redo",
                              command=lambda:controller.undo_manager.redo(),
                              state=DISABLED)
        # self.menu.add_command(label="debug", command=lambda:toggle_debug())
        self.menu.add_command(label="quit", command=controller.quit)
        self.menu.add_command(label="help", command=controller.help)
        master.config(menu=self.menu)
        # Canvas:
        self.spc = ScanpathCanvas(self, controller, width=350,
                                  height=350, background="white")
        self.spc.pack(side=LEFT, fill=BOTH, expand=1)
        # Meter:
        self.meter_frame = Tkinter.Frame(self)
        Tkinter.Label(self.meter_frame, text="Metric:").pack()
        self.metric_var = Tkinter.StringVar(self.meter_frame)
        self.metric_var.set("Scasim")
        Tkinter.OptionMenu(self.meter_frame, self.metric_var,
                           "Scasim", "Levenshtein").pack()
        self.meter = Meter(self.meter_frame, self.controller,
                           self.metric_var, width=20, relief=RIDGE, bd=3,
                           background="darkgrey")
        self.meter.pack(fill=Y, expand=1)
        self.meter_frame.pack(fill=Y, expand=1, padx=10, pady=10)
        sc1, sc2 = self.controller.scanpaths()
        sc1.register_coordinates(self.scanpath_modified)
        sc2.register_coordinates(self.scanpath_modified)
        sc1.register_add_fixation(self.scanpath_modified)
        sc2.register_add_fixation(self.scanpath_modified)
        sc1.register_delete_fixation(self.scanpath_modified)
        sc2.register_delete_fixation(self.scanpath_modified)
        # Bindings:
        master.bind('q', lambda e:controller.quit())
        master.bind('<Control-z>', lambda e: controller.undo_manager.undo())
        # FIXME How to specify this key combination?
        master.bind('<Shift-Control-z>', lambda e:controller.undo_manager.redo())
        master.bind('<Control-s>', lambda e: controller.write_file())
    def enable_save_button(self):
        self._change_menu_item_status(1, NORMAL)
    def disable_save_button(self):
        self._change_menu_item_status(1, DISABLED)
    def enable_saveas_button(self):
        self._change_menu_item_status(2, NORMAL)
    def disable_saveas_button(self):
        self._change_menu_item_status(2, DISABLED)
    def enable_undo_button(self):
        self._change_menu_item_status(3, NORMAL)
    def disable_undo_button(self):
        self._change_menu_item_status(3, DISABLED)
    def enable_redo_button(self):
        self._change_menu_item_status(4, NORMAL)
    def disable_redo_button(self):
        self._change_menu_item_status(4, DISABLED)
    def _change_menu_item_status(self, idx, state):
        # NOTE This is necessary because the menu flickers when we
        # change states:
        if self.menu.entrycget(idx, "state") != state:
           self.menu.entryconfig(idx, state=state)
    def scanpath_modified(self, sc, *pos):
        self.enable_saveas_button()
        if self.controller.current_file is not None:
          self.enable_save_button()
    
class ScanpathCanvas(Tkinter.Canvas):
    def __init__(self, master, controller, **opts):
        Tkinter.Canvas.__init__(self, master, **opts)
        self.master = master
        self.controller = controller
        # TODO Update only sc1.
        self.controller.sc1.register_coordinates(self.scanpath_modified)
        self.controller.sc2.register_coordinates(self.scanpath_modified)
        self.controller.sc1.register_add_fixation(self.fixation_added)
        self.controller.sc2.register_add_fixation(self.fixation_added)
        self.controller.sc1.register_delete_fixation(self.fixation_deleted)
        self.controller.sc2.register_delete_fixation(self.fixation_deleted)
        # Sample text:
        #self.create_text(040, 350, text="Since jay always jogs a mile seems like a short distance to him.", font=("Arial", 20), anchor="w", fill="gray40")
        # Draw grid:
        for i in range(20):
            self.create_line(0, i*100, 2000, i*100, width=1, fill="lightgrey", dash=(4,4), tags="grid")
            self.create_text(5, i*100+10, anchor=W, text="%s"%(i*100), fill="grey", tags="axis")
        for j in range(1,20):
            self.create_line(j*100, 0, j*100, 2000, width=1, fill="lightgrey", dash=(4,4), tags="grid")
            self.last_grid_idx = self.create_text(j*100+5, 10, anchor=W, text="%s"%(j*100), fill="grey", tags="axis")
        self.fixation_ids = {self.controller.sc1:[], self.controller.sc2:[]}
        # Bindings:
        self.bind('<Double-Button-1>',
                  controller.add_fixation)
        self.bind('<Double-Shift-Button-1>',
                  controller.delete_fixation)
        self.bind('<Shift-Button-1>',
                  controller.start_move_fixation)
        self.bind('<Button-1>',
                  controller.start_move_scanpath)
        self.bind('<MouseWheel>',
                  controller.scale_fixation)
        self.bind('<Button-4>',
                  controller.scale_fixation_up)
        self.bind('<Button-5>',
                  controller.scale_fixation_down)
        #self.bind('<MouseWheel>', lambda e: pront(r))
    def fixation_added(self, sc, pos):
        # Add fixation and line segment:
        Id = self.create_oval(fixation_coords(sc[pos]),
                              tags=("%s_fix"%sc.name, "fix"), width=2,
                              fill="lightgrey", outline="grey")
        self.fixation_ids[sc].insert(pos, Id)
        if len(sc) == 2:
            self.create_line(coords(sc), tags=sc.name, width=3,
                             activefill="red", capstyle=ROUND,
                             joinstyle=MITER, arrow=LAST,
                             arrowshape=(16,20,6))
        self.tag_raise(sc.name, max(self.fixation_ids[sc]))
        if len(sc) > 2:
            # If there already is a line, update, because the number
            # of segments has changed.
            self.scanpath_modified(sc)
        if min(len(self.controller.sc1), len(self.controller.sc2)):
            self.update_alignment()
    def fixation_deleted(self, sc, pos):
        Id = self.fixation_ids[sc][pos]
        # delete fixation circle:
        self.delete(Id)
        del self.fixation_ids[sc][pos]
        # deal with scanpath line:
        if len(sc) == 1:
            self.delete(sc.name)
        else:
            self.scanpath_modified(sc)
        self.update_alignment()
    def scanpath_modified(self, sc):
        # Update scanpaths:
        if sc is not None:
            self.coords(sc.name, coords(sc))
            for pos, f in enumerate(sc):
                self.coords(self.fixation_ids[sc][pos], fixation_coords(f))
        self.update_alignment()
    def update_alignment(self):
        self.delete("matchline")
        # Setup new match lines:
        sc1, sc2 = self.controller.scanpaths()
        path = self.controller.edit_path
        if path is not None:
            i = len(sc1)
            j = len(sc2)
            while i>0 and j>0:
                if path[i][j] == 0:
                    i = i - 1
                elif path[i][j] == 1:
                    j = j - 1
                elif path[i][j] == 2:
                    self.create_line(sc1[i-1][0], sc1[i-1][1],
                                     sc2[j-1][0], sc2[j-1][1],
                                     tags="matchline", fill="red", dash=(4, 4), width=2)
                    i, j = i-1, j-1
    def dnd_enter(self, source, event):
        # At which fixations do we start?
        self.dnd_origin_x = event.x
        self.dnd_origin_y = event.y
        self.dnd_last_x = event.x
        self.dnd_last_y = event.y
    def dnd_leave(self, source, event):
        pass
    def dnd_accept(self, source, event):
        return source in (self.controller.sc1, self.controller.sc2) and self
    def dnd_motion(self, source, event):
        dx = event.x - self.dnd_last_x
        dy = event.y - self.dnd_last_y
        source._translate(dx, dy)
        self.dnd_last_x = event.x
        self.dnd_last_y = event.y
    def dnd_commit(self, source, event):
        dx = event.x - self.dnd_origin_x
        dy = event.y - self.dnd_origin_y
        command = source._command(dx, dy)
        self.controller.undo_manager.done(command)

# This widget shows a visual representation of a magnitude using a bar
# with varying length.
class Meter(Tkinter.Canvas):
    def __init__(self, master, controller, metric_var, fillcolor="green", **opts):
        Tkinter.Canvas.__init__(self, master, **opts)
        self.rectangle = self.create_rectangle(0, 0,
                                               self.winfo_reqwidth(),
                                               self.winfo_reqheight(),
                                               fill=fillcolor, width=0)
        self.bind('<Configure>', self.update_coords)
        self.controller = controller
        self.controller.sc1.register_coordinates(self.scanpath_modified)
        self.controller.sc2.register_coordinates(self.scanpath_modified)
        self.controller.sc1.register_add_fixation(self.fixation_added)
        self.controller.sc2.register_add_fixation(self.fixation_added)
        self.controller.sc1.register_delete_fixation(self.fixation_deleted)
        self.controller.sc2.register_delete_fixation(self.fixation_deleted)
        self.max_func = lambda s,t:s.duration()+t.duration()
        metric_var.trace("w", self.metric_changed)
    def metric_changed(self, *args):
        print "meter metric changed"
        if self.controller.main_frame.metric_var.get() == "Levenshtein":
            self.max_func = lambda s,t:(len(s)+len(t))/2.0
        else:
            self.max_func = lambda s,t:s.duration()+t.duration()
        self.scanpath_modified()
    def scanpath_modified(self, sc=None):
        sc1, sc2 = self.controller.scanpaths(sc)
        self.maximum = self.max_func(sc1, sc2)
        self.update_coords()
    def fixation_added(self, sc, pos):
        self.scanpath_modified(sc)
    def fixation_deleted(self, sc, pos):
        self.scanpath_modified(sc)
    def update_coords(self, event=None):
        if self.maximum:
            r = self.controller.similarity_value / self.maximum
            self.coords(self.rectangle, 0, self.winfo_height(), self.winfo_width(), self.winfo_height() - self.winfo_height()*r)
        else:
            pass

#
# Model:
#

class Scanpath:
    def __init__(self, name, fixations=None):
        self.name = name
        if fixations is None:
           self.fixations = []
        else:
           self.fixations = fixations
        self.coordinates_observers = []
        self.add_fixation_observers = []
        self.delete_fixation_observers = []
    def __str__(self):
        return list(self).__str__()
    def __repr__(self):
        s = []
        for x,y,d in self:
            s.append("%f\t%f\t%f\t%s" % (x, y, d, self.name))
        return "\n".join(s)
    def __iter__(self):
        return iter(self.fixations)
    def __len__(self):
        return len(self.fixations)
    def __getitem__(self, pos):
        return self.fixations[pos]
    def __setitem__(self, pos, fix):
        self.fixations[pos] = fix
        self.notify_coordinates_observers()
    def __add__(self, other):
        sc = Scanpath("temp")
        for x,y,d in list(self) + list(other):
            sc.add_fixation(len(sc), x, y, d)
        return sc
    def register_coordinates(self, observer):
        self.coordinates_observers.append(observer)
    def unregister_coordinates(self, observer):
        self.coordinates_observers.remove(observer)
    def notify_coordinates_observers(self):
        for observer in self.coordinates_observers:
            observer(self)
    def register_add_fixation(self, observer):
        self.add_fixation_observers.append(observer)
    def unregister_add_fixation(self, observer):
        self.add_fixation_observers.remove(observer)
    def notify_add_fixation_observers(self, pos):
        for observer in self.add_fixation_observers:
            observer(self, pos)
    def register_delete_fixation(self, observer):
        self.delete_fixation_observers.append(observer)
    def unregister_delete_fixation(self, observer):
        self.delete_fixation_observers.remove(observer)
    def notify_delete_fixation_observers(self, pos):
        for observer in self.delete_fixation_observers:
            observer(self, pos)
    def translate(self, dx=0, dy=0):
        self.fixations = [(x+dx, y+dy, d) for x,y,d in self.fixations]
        self.notify_coordinates_observers()
    def translate_fixation(self, pos, dx, dy):
        x, y, d = self.fixations[pos]
        self.fixations[pos] = (x+dx, y+dy, d)
        self.notify_coordinates_observers()
    def add_fixation(self, pos, x, y, d):
        if pos==None:
            pos = len(self.fixations)
        self.fixations.insert(pos, (float(x),float(y),float(d)))
        self.notify_add_fixation_observers(pos)
    def delete_fixation(self, pos):
        del self.fixations[pos]
        self.notify_delete_fixation_observers(pos)
    def closest_fixation(self, *p):
        ds = [euclidean(p1, p) for p1 in self.fixations]
        return min(xrange(len(ds)), key=ds.__getitem__), min(ds)
    def duration(self):
        return sum([d for x,y,d in self])
    def dnd_end(self, target, event):
        pass
    def closest_segment(self, x, y):
        return self._distance(x, y)[1]
    def distance(self, x, y):
        return self._distance(x, y)[0]
    def _distance(self, x, y):
        # See http://local.wasp.uwa.edu.au/~pbourke/geometry/pointline/
        if len(self) == 0:
            return sys.maxint, 0
        if len(self) == 1:
            return euclidean((x,y), self[0]), 0
        dist = sys.maxint
        for i in range(len(self)-1):
            x1, y1, _ = self[i]
            x2, y2, _ = self[i+1]
            u = ((x - x1) * (x2 - x1) + (y - y1) * (y2 - y1)) / ((x1-x2)**2 + (y1-y2)**2)
            if u < 0:
               d = euclidean((x1,y1), (x,y))
            elif u > 1:
               d = euclidean((x2,y2), (x,y))
            else:
               xt = x1 + u * (x2 - x1)
               yt = y1 + u * (y2 - y1)
               d = euclidean((xt,yt), (x,y))
            if d < dist:
                dist = d
                closest_segment = i
        return dist, closest_segment

#
# Controller:
#
    
class ScanpathController:
    def __init__(self, tk_root):
        self.tk_root = tk_root
        self.sc1 = Scanpath("sc1")
        self.sc2 = Scanpath("sc2")
        # It's important that the controller is notified first because
        # it calculates scasim scores and the GUI would show old
        # values if the controller was notified later.
        self.sc1.register_coordinates(self.scanpath_modified)
        self.sc2.register_coordinates(self.scanpath_modified)
        self.sc1.register_add_fixation(self.scanpath_modified)
        self.sc2.register_add_fixation(self.scanpath_modified)
        self.sc1.register_delete_fixation(self.scanpath_modified)
        self.sc2.register_delete_fixation(self.scanpath_modified)
        self.current_file = None
        self.unsaved_changes = None
        self.metric = scasim
        self.similarity_value = None
        self.edit_path = None
        self.undo_manager = UndoManager(self)
        self.message = None
        self.main_frame = MainFrame(tk_root, self)
        self.main_frame.metric_var.trace("w", self.metric_changed)
        self.main_frame.pack(fill=BOTH, expand=1)
        for x,y,d in ((100,100,100), (200,200,150), (300,100, 80)):
            self.sc1.add_fixation(len(self.sc1), x, y, d)
        for x,y,d in ((100,200,100), (200,300,130), (300,300, 90)):
            self.sc2.add_fixation(len(self.sc2), x, y, d)
        self.main_frame.disable_save_button()
        self.main_frame.disable_saveas_button()
    def scanpaths(self, sc=None):
        if sc is None or sc in (self.sc1, self.sc2):
            return self.sc1, self.sc2
        else:
            return sc, self.sc1.name == sc.name and self.sc2 or self.sc1
    def metric_changed(self, *args):
        print "conroller metric changed"
        if self.main_frame.metric_var.get() == "Levenshtein":
            self.metric = levenshtein
        else:
            self.metric = scasim
        self.similarity_value, self.edit_path = self.metric(self.sc1, self.sc2, 210, 210, 70, 1.0/50)
        self.main_frame.spc.update_alignment()
    def scanpath_modified(self, sc=None, *l):
        sc1, sc2 = self.scanpaths(sc)
        # TODO Get real center of screen.  Also other values.
        self.similarity_value, self.edit_path = self.metric(sc1, sc2, 210, 210, 70, 1.0/50)
        self.unsaved_changes = True
    def find_closest_scanpath(self, x, y):
        d1 = self.sc1.distance(x,y)
        d2 = self.sc2.distance(x,y)
        if d1 < d2:
            return self.sc1
        return self.sc2
    def add_fixation(self, event, d=100):
        x, y = event.x, event.y
        sc = self.find_closest_scanpath(x, y)
        # Insert at which fixation?
        if len(sc) <= 1:
            self.undo_manager.do(AddFixationCommand(sc, len(sc), x, y, d))
        else:
            seg_pos = sc.closest_segment(x, y)
            # NOTE There is a tiny bug lurking here: The x,y position
            # can be above the segment but still have the same
            # distance to the segments as to the next fixation.  In
            # this rare case the new fixation will be appended to the
            # scanpath although it should be inserted in the last
            # segment.
            if seg_pos == len(sc)-2 and sc.distance(x, y) == euclidean(sc[-1], (x,y)):
                self.undo_manager.do(AddFixationCommand(sc, len(sc), x, y, d))
            elif seg_pos == 0 and sc.distance(x, y) == euclidean(sc[0], (x,y)):
                self.undo_manager.do(AddFixationCommand(sc, 0, x, y, d))
            else:
                self.undo_manager.do(AddFixationCommand(sc, seg_pos+1, x, y, d))
    def scale_fixation_up(self, event):
        self._scale_fixation(event, 1)
    def scale_fixation_down(self, event):
        self._scale_fixation(event, -1)
    def scale_fixation(self, event):
        self._scale_fixation(event, event.delta)
    def _scale_fixation(self, event, value):
        x, y = event.x, event.y
        sc = self.find_closest_scanpath(x, y)
        pos,_ = sc.closest_fixation(x, y)
        self.undo_manager.do(ScaleFixationCommand(sc, pos, value*0.1))
    def delete_fixation(self, event, sc=None):
        x, y = event.x, event.y
        if sc is None:
            sc = self.find_closest_scanpath(x, y)
        pos,_ = sc.closest_fixation(x, y)
        if len(sc) > 1:
            self.undo_manager.do(DeleteFixationCommand(sc, pos))
    def start_move_scanpath(self, event):
        sc = self.find_closest_scanpath(event.x, event.y)
        # NOTE This mess could be avoided if fixations were objects of their own.
        sc._translate = sc.translate
        sc._command = lambda dx, dy: TranslateScanpathCommand(sc, dx, dy)
        Tkdnd.dnd_start(sc, event)
    def start_move_fixation(self, event):
        # TODO Does this really belong here?
        # What is the closest fixation?
        i1, d1 = self.sc1.closest_fixation(event.x, event.y)
        i2, d2 = self.sc2.closest_fixation(event.x, event.y)
        if (d1 < d2):
            sc = self.sc1
            i = i1
        else:
            sc = self.sc2
            i = i2
        sc._translate = lambda dx, dy:sc.translate_fixation(i, dx, dy)
        sc._command = lambda dx, dy: TranslateFixationCommand(sc, i, dx, dy)
        Tkdnd.dnd_start(sc, event)
    def open_file(self, f):
        if f == '':
            return
        if type(f) == str:
            f = file(f, 'r')
        self.clear()
        sc = None
        f.readline() # remove head
        for d, x, y, trial in [l.split('\t') for l in f]:
            if sc is None:
                sc = self.sc1
                sc.name = trial
            elif sc == self.sc1 and sc.name != trial:
                sc = self.sc2
                sc.name = trial
            elif sc == self.sc2 and sc.name != trial:
                message("There are more than two trials in the file.  Using only the first two.")
                break
            sc.add_fixation(len(sc), x, y, d)
        f.close()
    def clear(self):
        for sc in self.scanpaths():
            for pos in range(len(sc)):
                sc.delete_fixation(0)
    def write_file(self, f=None):
        if f == '':
            return
        if f is None:
            f = self.current_file
        if f is None:
            f = self.main_frame.save_dialog.show()
        if type(f) == str:
            f = file(f, 'w')
        f.write("d\tx\ty\ttrial\n")
        f.write(self.sc1.__repr__())
        f.write("\n")
        f.write(self.sc2.__repr__())
        f.write("\n")
        f.close()
        self.current_file = f.name
        self.main_frame.disable_save_button()
        self.main_frame.disable_saveas_button()
    def quit(self):
        self.write_file(sys.stdout)
        self.tk_root.destroy()
    def help(self):
        if self.message==None:
            self.message = Tkinter.Message(self.tk_root, text="""The bar at the right shows the dissimilarity of the two scanpaths as a proportion of the total time of the scanpaths.  If the Levenshtein metric is used, the bar shows the edit-distance divided by the number total number of fixations.  Scanpaths can be moved via drag-and-drop.  Fixations can be moved via drag-and-drop while pressing the shift key.  The duration of fixations can be changed by placing the mouse pointer on them and then using the mouse wheel.  New fixations can be created by double-clicking on the canvas at the position where you want them.  Fixations can be deleted by double-clicking on them while pressing the shift key.  When switching the metric to Levenshtein, the quadrants of the grid define the regions of interest.  Keyboard shortcuts: 'control + s' saves the scanpaths in tab-separated-values (tsv) format, 'q' is quit, 'control + z' is undo, 'control + shift + z' is redo.  Click 'help' again to close this message.""")
            self.message.pack()
        else:
            self.message.destroy()
            self.message = None

#
# Undo system:
#

class UndoManager:
    def __init__(self, controller):
        self.controller = controller
        self.history = []
        self.future = []
    def do(self, command):
        command.execute()
        self.done(command)
    def done(self, command):
        self.future = []
        self.history.append(command)
        self.controller.main_frame.disable_redo_button()
        self.controller.main_frame.enable_undo_button()
    def undo(self):
        if self.history:
            command = self.history.pop()
            command.undo()
            self.future.append(command)
            self.controller.main_frame.enable_redo_button()
            if not self.history:
                self.controller.main_frame.disable_undo_button()
            else:
                self.controller.main_frame.enable_undo_button()
        else:
            raise Exception("Nothing to undo.")
    def redo(self):
        if self.future:
            command = self.future.pop()
            command.execute()
            self.history.append(command)
            self.controller.main_frame.enable_undo_button()
            if not self.future:
                self.controller.main_frame.disable_redo_button()
            else:
                self.controller.main_frame.enable_redo_button()
        else:
            raise Exception("Nothing to redo.")

# NOTE Do the Commands need a common super class?  I'd say no, because
# there is no that could be shared at all.  The interface is shared
# but, hey, this is not a strongly typed language and we shouldn't try
# to emulate Java.
class TranslateFixationCommand:
    def __init__(self, sc, pos, dx, dy):
        self.sc = sc
        self.pos = pos
        self.dx = dx
        self.dy = dy
    def execute(self):
        self.sc.translate_fixation(self.pos, self.dx, self.dy)
    def undo(self):
        self.sc.translate_fixation(self.pos, -self.dx, -self.dy)
    def __str__(self):
        return "Move fixation %d in scanpath %s by %d, %d pixels." % (self.pos, self.sc.name, self.dx, self.dy)
    
class TranslateScanpathCommand:
    def __init__(self, sc, dx, dy):
        self.sc = sc
        self.dx = dx
        self.dy = dy
    def execute(self):
        self.sc.translate(self.dx, self.dy)
    def undo(self):
        self.sc.translate(-self.dx, -self.dy)
    def __str__(self):
        return "Move scanpath %s by %d, %d pixels." % (self.sc.name, self.dx, self.dy)

class AddFixationCommand:
    def __init__(self, sc, pos, x, y, d):
        self.sc = sc
        self.pos = pos
        self.x = x
        self.y = y
        self.d = d
    def execute(self):
        self.sc.add_fixation(self.pos, self.x, self.y, self.d)
    def undo(self):
        self.sc.delete_fixation(self.pos)
    def __str__(self):
        return "Add fixation at position %d in scanpath %s at position (%s, %s) with %s ms duration." % (self.pos, self.sc.name, self.x, self.y, self.d)
        
class DeleteFixationCommand:
    def __init__(self, sc, pos):
        self.sc = sc
        self.pos = pos
    def execute(self):
        self.x, self.y, self.d = self.sc[self.pos]
        self.sc.delete_fixation(self.pos)
    def undo(self):
        self.sc.add_fixation(self.pos, self.x, self.y, self.d)
    def __str__(self):
        return "Delete fixation at position %d in scanpath %s." % (self.pos, self.sc.name)
        
class ScaleFixationCommand:
    def __init__(self, sc, pos, factor):
        self.sc = sc
        self.pos = pos
        self.factor = factor
    def execute(self):
        self.x, self.y, self.d = self.sc[self.pos]
        self.sc[self.pos] = (self.x, self.y, self.d + self.d*self.factor)
    def undo(self):
        self.sc[self.pos] = (self.x, self.y, self.d)
    def __str__(self):
        return "Change duration of fixation at position %d in scanpath %s by %s." % (self.pos, self.sc.name, self.d*self.factor)
        
        
#
# Misc:
#
        
def message(sep=" ", *messages):
    sys.stderr.write(sep.join(messages))
    sys.stderr.write('\n')

def coords(sc):
    xy = [(x,y) for (x,y,_) in sc]
    return reduce(lambda t1,t2: t1+t2, xy, ())

def fixation_coords(fixation, dx=0, dy=0):
    """Generate coordinates for the circle representing a fixation."""
    x,y,d = fixation
    # TODO The scale factor should not be hard-coded.
    d = d * 0.2
    return (dx+x-d, dy+y-d, dx+x+d, dy+y+d)

def levenshtein(s, t, *opts):

    # Prepare scanpaths:
    
    letters = "abcdefghijklmnopqrstuvwxyz"
    s = [letters[int(x)/100]+letters[int(y)/100] for x,y,d in s]
    t = [letters[int(x)/100]+letters[int(y)/100] for x,y,d in t]
    
    ns, nt = len(s), len(t)

    # Prepare similarity matrix:
    
    d = map(lambda i:[0]*(nt+1), xrange(ns+1))
    for i in xrange(ns+1):
        d[i][0] = i
    d[0] = range(nt+1)
    
    # Prepare matrix for keeping track of the edit
    # operations.  (Needed for determining the alignment.)
        
    path = map(lambda i:[0]*(nt+1), xrange(ns+1))
    for i in xrange(1,ns+1):
        path[i][0] = 1
    for j in xrange(1,nt+1):
        path[0][j] = 0
        
    for i in xrange(1,ns+1):
        for j in xrange(1,nt+1):
            cost = s[i-1]!=t[j-1] and 1 or 0
            ops = (d[i-1][j]+1, d[i][j-1]+1, d[i-1][j-1]+cost)
            mi = which_min(*ops)
            path[i][j] = mi
            d[i][j] = ops[mi]
    
    return d[-1][-1], path

# TODO Pass individual values not tuples.  This is consistent with
#      how other functions receive coordinates.
def euclidean(p1, p2):
    return sqrt((p1[0]-p2[0])**2 + (p1[1]-p2[1])**2)

def inverse_gnomonic(sc, center_x, center_y, distance, unit_size=1):

    res = []

    for x,y,d in sc:
        x = (x - center_x) * unit_size / distance
        y = (y - center_y) * unit_size / distance

        rho = sqrt(x**2 + y**2)
        c   = atan(rho)

        # FIXME At point 0,0 we get NaNs.
        
        # NOTE 180/pi converts radians to degrees
        # NOTE The formulae simplify because we set phi_1 and
        #      lambda_0 to 0.  See the above-mentioned link.

        lat   = asin(y * sin(c) / rho) * 180/pi
        lon   = atan2(x * sin(c), rho * cos(c)) * 180/pi

        res.append((lon, lat, d))

    return res

def scasim(s, t, center_x, center_y, distance, unit_size, modulator=0.83):

    if len(s) == 0:
        return t.duration(), None
    if len(t) == 0:
        return s.duration(), None
    
    s = inverse_gnomonic(s, center_x, center_y, distance, unit_size)
    t = inverse_gnomonic(t, center_x, center_y, distance, unit_size)

    # Prepare similarity matrix:
    
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

    # Prepare matrix for keeping track of the edit
    # operations.  (Needed for determining the alignment.)
        
    path = map(lambda i:[0]*(n+1), xrange(m+1))
    for i in xrange(1,m+1):
        path[i][0] = 1
    for j in xrange(1,n+1):
        path[0][j] = 0
    
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
            
            path[j+1][i+1] = mi
            
            d[j+1][i+1] = ops[mi]
            
    # print d[-1][-1]
    return d[-1][-1], path

def which_min(*l):
    mi = 0
    for i,e in enumerate(l):
        if e<l[mi]:
            mi = i
    return mi

if __name__ == '__main__':
  
    tk_root = Tkinter.Tk()
    controller = ScanpathController(tk_root)
    # if len(sys.argv)==2 and os.path.exists(sys.argv[1]):
    #     controller.open_file(sys.argv[1])
    tk_root.mainloop()
      
    if False:
        sc1 = Scanpath("sc1")
        sc2 = Scanpath("sc2")
        sc1.add_fixation(100, 100, 100)
        sc1.add_fixation(200, 200, 100)
        sc1.add_fixation(300, 100, 100)
        sc2.add_fixation(100, 200, 100)
        sc2.add_fixation(200, 300, 100)
        sc2.add_fixation(300, 300, 100)



