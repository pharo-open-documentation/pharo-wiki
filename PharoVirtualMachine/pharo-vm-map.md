
# Introduction

## design principles behind smalltalk
https://www.cs.virginia.edu/~evans/cs655/readings/smalltalk.html

**"*Personal Mastery*: If a system is to serve the creative spirit, it must be entirely comprehensible to a single individual. "**

The history of opensmalltalk/pharo virtual machine started a long time ago, with Smalltalk-80, and then in the squeak virtual machine, now opensmalltalk.

http://files.squeak.org/docs/OOPSLA.Squeak.html

"Squeak is an open, highly-portable Smalltalk implementation whose virtual machine is written entirely in Smalltalk, making it easy to debug, analyze, and change. To achieve practical performance, a translator produces an equivalent C program whose performance is comparable to commercial Smalltalks."

## overview
Two major components of the Smalltalk-80 system can be distinguished: the virtual image and the virtual machine.

1. The virtual image consists of all of the objects in the system, including Smalltalk source code objects, compiler objects, and bytecode compiled method objects. It is platform independent.
1. The virtual machine is the engine that runs the image. It consists of a machine language (or microcode or bytecode) routines that give dynamics to the objects in the virtual image. It is a platform dependent machine code executable that can be decomposed into: 
    * *Bytecode execution engine* - to interpret or JIT (Just In Time) compile each bytecode into machine code and execute it;  
    * *Object engine* - to provide memory management of objects;   
    * *Primitives* - to provide AOT (Ahead Of Time) compiled machine code for accelerated execution and interface to platform dependent operating system routines."


The system implementer's task is to create a virtual machine. A virtual image can then be loaded into the virtual machine and the pharo system becomes an interactive entity.

The various link of the opensmalltalk/pharo VM implementation given in this chapter is organized in a top-down fashion, starting with the source methods written by programmers. These methods are translated by a *compiler* into sequences of instructions called bytecodes. The bytecodes produced by the compiler are instructions for an *interpreter*. Below the interpreter in the implementation is an *object memory* that stores the objects that make up the virtual image.





Main lecture: Smalltalk - 80. The language and its implementation:
http://sdmeta.gforge.inria.fr/FreeBooks/BlueBook/Bluebook.pdf

Especially, the part that are related to the VM: pages 541 to pages 691
A web format can be found here:
http://www.mirandabanda.org/bluebook/

A tour of the squeak object engine show a good overview, but is a bit more outdated:
http://stephane.ducasse.free.fr/FreeBooks/CollectiveNBlueBook/Rowledge-Final.pdf

Bytecode and Virtual Machines:
http://scg.unibe.ch/download/lectures/cc2011/09BytecodeVirtualMachines.pptx.pdf

An overview of all the the changes brough to opensmalltalk/pharo VM:
https://www.researchgate.net/publication/328509577_Two_Decades_of_Smalltalk_VM_Development_Live_VM_Development_through_Simulation_Tools

## set up
http://forum.world.st/ANN-Pharo-Headless-Beta-Actually-what-is-between-Alpha-and-Beta-td5102089.html

headless VM: https://github.com/pharo-project/opensmalltalk-vm located in the headless branch.

You'll find instruction on how to build it on the github page. In addition, you'll get an image with VMMaker loaded so you can study the code of the VM.

Tips on debugging the VM:
* https://pharoweekly.wordpress.com/2019/08/28/pharo-headless-linux-vm-debugging/
* https://clementbera.wordpress.com/2016/05/30/simulating-the-cog-vm/


# Compiler, bytecodes and compiled method
## introduction
https://marianopeck.blog/2011/04/30/smalltalk-reflective-model/

## compiler - Opal
Source methods written by programmers are represented in the system as instances of *String*. The Strings contain sequences of characters that conform to the pharo syntax.

Source methods are translated by the system's *compiler* into sequences of instructions. The instructions are called bytecodes.

* Towards a flexible Pharo Compiler: https://hal.inria.fr/hal-00862411/document
* https://rmod.inria.fr/archives/events/2011DIS/Slides/Opal/OpalCompiler.pdf
* https://clementbera.wordpress.com/2013/03/22/toward-a-zero-bugs-interpreter-compiler/
* https://clementbera.wordpress.com/2013/05/28/modular-compilation-with-opal/

### Example of live compiling
* http://forum.world.st/Squeak-Compiler-evaluate-3-4-Pharo-equivalent-td5086475.html
* http://forum.world.st/evaluate-with-bindings-td5071919.html


## bytecode:
The original smalltalk interpreter understands 256 bytecode instructions that fall into five categories: pushes, stores, sends, returns, and jumps. Since more than 256 instructions for the interpreter are needed, some of the bytecodes take extensions. An extension is one or two bytes following the bytecode that further specify the instruction. An extension is not an instruction on its own, it is only a part of an instruction. 

* https://marianopeck.blog/2011/05/21/introduction-to-smalltalk-bytecodes/
* https://clementbera.wordpress.com/2013/04/21/byte-code-to-source-code-mappin/
* https://clementbera.wordpress.com/2013/09/23/squeakv3plusclosure-bytecode-set/
* A bytecode set for adaptive optimizations: https://hal.inria.fr/hal-01088801/document

## compiled method:
The compiler creates an instance of *CompiledMethod* to hold the bytecode translation of a source method. 

* https://marianopeck.blog/2011/05/14/playing-with-compiledmethod/
* https://marianopeck.blog/2011/06/03/primitives-pragmas-literals-and-their-relation-to-compiledmethods/
* https://clementbera.wordpress.com/2013/07/29/smalltalk-method-and-block-models/
* http://www.mirandabanda.org/cogblog/2008/06/17/bluebook-compiledmethods-having-our-cake-and-eating-it-too/
* https://marianopeck.blog/2011/07/06/named-primitives/




## class format:
https://marianopeck.blog/2011/05/07/class-formats-and-compiledmethod-uniqueness/




# stack interperter
The original smalltalk system wat a stack-oriented interpreter. A big part of the system still contain its source code, which is enhanced with Just In Time  (JIT) in recent version.

The Smalltalk-80 interpreter executes the bytecode instructions found in *CompiledMethods*. The interpreter uses five pieces of information and repeatedly performs a three-step cycle. 

More information: 
http://www.mirandabanda.org/bluebook/bluebook_chapter26.html#TheInterpreter26

Understand how a call stack interperter is working:
https://files.pharo.org/books-pdfs/booklet-CallStack/2018-01-23-CallStack.pdf

Understand how a simple system may work:
http://books.pharo.org/booklet-ReflectiveCore/pdf/2018-03-05-ReflectiveKernel-spiral.pdf


## context
The interpreter uses *contexts* to represent the state of its execution of *CompiledMethods* and *blocks*. A context can be a *MethodContext* or a *BlockContext*. A MethodContext represents the execution of a CompiledMethod that was invoked by a message.

* http://www.mirandabanda.org/bluebook/bluebook_chapter26.html#Contexts26

* http://www.mirandabanda.org/bluebook/bluebook_chapter27.html#Contexts27

### in Opensmalltalk/Pharo
http://www.mirandabanda.org/cogblog/2009/01/14/under-cover-contexts-and-the-big-frame-up/

## block closure
### In the original smalltalk-80 system:
http://www.mirandabanda.org/bluebook/bluebook_chapter26.html#BlockContexts26


### In Opensmalltalk/pharo
* http://www.mirandabanda.org/cogblog/2008/06/07/closures-part-i/
* http://www.mirandabanda.org/cogblog/2008/07/22/closures-part-ii-the-bytecodes/
* http://www.mirandabanda.org/cogblog/2008/07/24/closures-part-iii-the-compiler/

* https://clementbera.wordpress.com/2016/06/27/fullblockclosure-design/

# object memory
The object memory provides the interpreter with an interface to the objects that make up the pharo virtual image. Each object is associated with a unique identifier called its object pointer. The object memory and interpreter communicate about objects with object pointers.

The object memory associates each object pointer with a set of other object pointers. Every object pointer is associated with the object pointer of a class. If an object has instance variables, its object pointer is also associated with the object pointers of their values. The individual instance variables are referred to by zero-relative integer indices. The value of an instance variable can be changed, but the class associated with an object cannot be changed. The object memory provides the following five fundamental functions to the interpreter.
http://www.mirandabanda.org/bluebook/bluebook_chapter26.html#TheObjectMemory26

### in Opensmalltalk/Pharo

* https://marianopeck.blog/2011/10/26/memory-addresses-and-immediate-objects/
* http://www.mirandabanda.org/cogblog/2013/09/05/a-spur-gear-for-cog/
* https://clementbera.wordpress.com/2014/01/16/spurs-new-object-format/
* https://clementbera.wordpress.com/2014/02/06/7-points-summary-of-the-spur-memory-manager/


# beyond smalltalk-80 original VM - JIT
## Just In Time - JIT
* http://www.mirandabanda.org/cogblog/2011/03/01/build-me-a-jit-as-fast-as-you-can/
* http://www.mirandabanda.org/cogblog/2011/03/04/an-arranged-marriage/

## memory compactor and garbage collection
* https://clementbera.wordpress.com/2018/06/07/free-chunk-management-in-the-cog-vm/
* https://clementbera.wordpress.com/2017/03/12/tuning-the-pharo-garbage-collector/

### Specific to Pharo
https://github.com/SquareBracketAssociates/Booklet-PharoVirtualMachine

## other
Sub-method, partial behavioral reflection with Reflectivity: Looking back on 10 years of use
https://programming-journal.org/2020/4/5/

Debugging the VM through the simulator (video)
* https://clementbera.wordpress.com/2018/03/07/sista-vm-screencast/

* http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/

## multi-user transactional image
magma object representation by Chris Muller. 
* https://wiki.squeak.org/squeak/2665.

# UFFI
* https://users.dcc.uchile.cl/~rsalgado/uffi-fourth-draft.pdf
* http://books.pharo.org/booklet-uffi/pdf/2020-02-12-uFFI-V1.0.1

Pharo threaded FFI
https://github.com/pharo-project/threadedFFI-Plugin


# parameters of the virtual machine

https://lists.pharo.org/pipermail/pharo-users_lists.pharo.org/2016-May/026175.html

found in
```smalltalk
VirtualMachine >> parameterAt: parameterIndex

	"parameterIndex is a positive integer corresponding to one of the VM's internal
	parameter/metric registers.  Answer with the current value of that register.
	Fail if parameterIndex has no corresponding register.
	VM parameters are numbered as follows:
	1	end (v3)/size(Spur) of old-space (0-based, read-only)
	2	end (v3)/size(Spur) of young/new-space (read-only)
	3	end (v3)/size(Spur) of heap (read-only)
	4	nil (was allocationCount (read-only))
	5	nil (was allocations between GCs (read-write)
	6	survivor count tenuring threshold (read-write)
	7	full GCs since startup (read-only)
	8	total milliseconds in full GCs since startup (read-only)
	9	incremental GCs (SqueakV3) or scavenges (Spur) since startup (read-only)
	10	total milliseconds in incremental GCs (SqueakV3) or scavenges (Spur) since startup (read-only)
	11	tenures of surving objects since startup (read-only)
	12-20 were specific to ikp's JITTER VM, now 12-19 are open for use
	20	utc microseconds at VM start-up (actually at time initialization, which precedes image load).
	21	root table size (read-only)
	22	root table overflows since startup (read-only)
	23	bytes of extra memory to reserve for VM buffers, plugins, etc (stored
	in image file header).
	24	memory threshold above which shrinking object memory (rw)
	25	memory headroom when growing object memory (rw)
	26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds	(rw) 27	number of times mark loop iterated for current IGC/FGC (read-only)	includes ALL marking
	28	number of times sweep loop iterated for current IGC/FGC (read-only)
	29	number of times make forward loop iterated for current IGC/FGC	(read-only) 30	number of times compact move loop iterated for current	IGC/FGC (read-only)
	31	number of grow memory requests (read-only)
	32	number of shrink memory requests (read-only)
	33	number of root table entries used for current IGC/FGC (read-only)
	34	number of allocations done before current IGC/FGC (read-only)
	35	number of survivor objects after current IGC/FGC (read-only)
	36	millisecond clock when current IGC/FGC completed (read-only)
	37	number of marked objects for Roots of the world, not including Root	Table entries for current IGC/FGC (read-only)
	38	milliseconds taken by current IGC (read-only)
	39	Number of finalization signals for Weak Objects pending when current	IGC/FGC completed (read-only)
	40	BytesPerOop for this image
	41	imageFormatVersion for the VM
	42	number of stack pages in use
	43	desired number of stack pages (stored in image file header, max 65535)
	44	size of eden, in bytes
	45	desired size of eden, in bytes (stored in image file header)
	46	machine code zone size, in bytes (Cog only; otherwise nil)
	47	desired machine code zone size (stored in image file header; Cog only;	otherwise nil)
	48	various header flags. See getCogVMFlags.
	49	max size the image promises to grow the external semaphore table to (0	sets to default, which is 256 as of writing)
	50-51 nil; reserved for VM parameters that persist in the image (such as	eden above)
	52	root table capacity
	53	number of segments (Spur only; otherwise nil)
	54	total size of free old space (Spur only, otherwise nil)
	55	ratio of growth and image size at or above which a GC will be performed	post scavenge
	56	number of process switches since startup (read-only)
	57	number of ioProcessEvents calls since startup (read-only)
	58	number of ForceInterruptCheck calls since startup (read-only)
	59	number of check event calls since startup (read-only)
	60	number of stack page overflows since startup (read-only)
	61	number of stack page divorces since startup (read-only)	62	compiled code compactions since startup (read-only; Cog only; otherwise nil)
	63	total milliseconds in compiled code compactions since startup	(read-only; Cog only; otherwise nil)
	64	the number of methods that currently have jitted machine-code
	65	whether the VM supports a certain feature, MULTIPLE_BYTECODE_SETS is bit 0, IMMTABILITY is bit 1
	66	the byte size of a stack page
	67	the max allowed size of old space (Spur only; nil otherwise; 0 implies	no limit except that of the underlying platform)
	68	the average number of live stack pages when scanned by GC (at	scavenge/gc/become et al)
	69	the maximum number of live stack pages when scanned by GC (at	scavenge/gc/become et al)
	70	the vmProxyMajorVersion (the interpreterProxy VM_MAJOR_VERSION)
	71	the vmProxyMinorVersion (the interpreterProxy VM_MINOR_VERSION)"
```
