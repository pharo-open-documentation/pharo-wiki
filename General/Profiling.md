# Profiling
Profiling a program is the act of measuring, for example, the time or space it takes to execute.

* [Time Profiling](#time-profiling)
  + [Method 1](#method-1)
  + [Method 2](#method-2)
  + [Method 3](#method-3)
* [Space Profiling](#space-profiling)
* [Virtual Machine Profiling](#virtual-machine-profiling)

## Time Profiling
### Method 1
If `SimpleGridExample new open` is the program you want to profile, run:
```Smalltalk
TimeProfiler spyOn: [ SimpleGridExample new open ]
```
Result: a breakdown of the methods in which the program spent time.

![image](Profiling_Image_TimeProfilerFromCommandLine.png)

### Method 2
If `SimpleGridExample new open` is the program you want to profile, **inspect**:
```Smalltalk
[ SimpleGridExample new open ] bench.
```
Result: the number of times the profiler was able to run the program per second.

![image](Profiling_Image_Bench.png)

### Method 3
You can access a UI for the time profiler tool via the menu.

![image](Profiling_Image_TimeProfilerMenuItem.png)

Result: type your code in the top box and click "Profile it".

![image](Profiling_Image_TimeProfilerToolUI.png)

## Space Profiling
Imagine you want to know how much instances of the classes Point and TextMorph there are in the system, and how much space they occupy. **Inspect**:
```Smalltalk
((SpaceTally new spaceTally: (Array with: TextMorph with: Point)) 
	asSortedCollection: [:a :b | a spaceForInstances > b spaceForInstances])
```
Result:

![image](Profiling_Image_SpaceTally.png)

## Virtual Machine Profiling
*TODO*
