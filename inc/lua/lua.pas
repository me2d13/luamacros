(*******************************************************************************
* Copyright (C) 1994-2012 Lua.org, PUC-Rio.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*******************************************************************************)

(******************************************************************************)
(* Lua library 5.2.3, Freepascal port.                                        *)
(* Copyright (C) 2013, Remy Baccino, Hexadecimal Technologies.                *)
(* Contact : rbaccino@hexnode.net.                                            *)
(******************************************************************************)
(* The TLua Class and its dependencies was originally written by Dennis D. Spreen,
   under the following copyright :

   * Copyright 2009  Dennis D. Spreen (email : dennis@spreendigital.de)
                                                                              *)
(* Developers of the first adaptation :

   *  2005 Rolf Meyerhoff
   *  www.matrix44.de

   Copyright for the Lua 5.1 adaptation:

   *  2007 Marco Antonio Abreu
   *  www.marcoabreu.eti.br                                                   *)
(******************************************************************************)
(* Both the code produced by Remy Baccino and Dennis D. Spreen are licensed
   under the GPL :

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
                                                                              *)
(******************************************************************************)
(* Changes :
		 * 2013-11-11 : Simply adjusted the lua.pas header to match the new
						lua 5.2.3 version, no additional changes necessary.
         * 2013-10-08 : fixed a crash on 32 bit systems when pushing/popping
                        from the stack. A low LUAI_MAXSTACK was the cause of it.

         * 2013-10-08 : added a new function RunFromMem in the TLua class.
                        It handles transparently the running of Lua text or
                        pre-compiled buffers. Also rewrote DoFile to use
                        function luaL_loadfile instead of the macro lua_dofile,
                        so that errors are better handled.
                                                                              *)
(******************************************************************************)

unit Lua;

{$IFDEF FPC}
        {$mode delphi}{$H+}
{$ENDIF}

interface

uses
    Classes, SysUtils

{$IFDEF FPC}
     ,dynlibs
{$ELSE}
    {$IFDEF WINDOWS},Windows {$ENDIF}
{$ENDIF}

{$IFDEF UNIX}
     ,unixtype
{$ENDIF}
;

{$DEFINE LUA_NUMBER_DOUBLE}

const
{$IFDEF WINDOWS}
     LUA_API = 'lua5.2.dll';
{$ELSE}
{$IFDEF UNIX}
     LUA_API = 'lua.so.5.2';
{$ENDIF}
{$ENDIF}

     LUA_VERSION_MAJOR     = '5';
     LUA_VERSION_MINOR     = '2';
     LUA_VERSION_NUM       = 502;
     LUA_VERSION_RELEASE   = '3';


     LUA_VERSION_          = 'Lua '+LUA_VERSION_MAJOR+'.'+LUA_VERSION_MINOR;
     LUA_RELEASE           = LUA_VERSION_+'.'+LUA_VERSION_RELEASE;
     LUA_COPYRIGHT         = LUA_RELEASE+' Copyright (C) 1994-2012 Lua.org, PUC-Rio';
     LUA_AUTHORS           = 'R. Ierusalimschy, L. H. de Figueiredo, W. Celes';


     (* mark for precompiled code ('<esc>Lua') *)
     LUA_SIGNATURE         = #33+'Lua';
     (* option for multiple returns in 'lua_pcall' and 'lua_call' *)
     LUA_MULTRET           = -1;

     (* minimum Lua stack available to a C function *)
     LUA_MINSTACK	   = 20;

     (* maximum stack available.
      * CHANGE it if you need a different limit. This limit is arbitrary;
      * its only purpose is to stop Lua to consume unlimited stack
      * space (and to reserve some numbers for pseudo-indices).
      *)
{$IFDEF CPU64}
     LUAI_MAXSTACK         = 1000000;
{$ELSE}
     LUAI_MAXSTACK         = 1000000; //C value : 15000
{$ENDIF}

     LUAI_FIRSTPSEUDOIDX   = (-LUAI_MAXSTACK - 1000);

     (* pseudo-indices *)
     LUA_REGISTRYINDEX     = LUAI_FIRSTPSEUDOIDX;


     (* Lua error constants *)
     LUA_OK                = 0;
     LUA_YIELD_            = 1;
     LUA_ERRRUN            = 2;
     LUA_ERRSYNTAX         = 3;
     LUA_ERRMEM            = 4;
     LUA_ERRGCMM           = 5;
     LUA_ERRERR            = 6;


     (* basic types *)
     LUA_TNONE		   = -1;
     LUA_TNIL		   = 0;
     LUA_TBOOLEAN          = 1;
     LUA_TLIGHTUSERDATA	   = 2;
     LUA_TNUMBER	   = 3;
     LUA_TSTRING	   = 4;
     LUA_TTABLE	           = 5;
     LUA_TFUNCTION	   = 6;
     LUA_TUSERDATA	   = 7;
     LUA_TTHREAD	   = 8;

     LUA_NUMTAGS	   = 9;


     (* predefined values in the registry *)
     LUA_RIDX_MAINTHREAD   = 1;
     LUA_RIDX_GLOBALS      = 2;
     LUA_RIDX_LAST	   = LUA_RIDX_GLOBALS;


     (* arithmetic operations *)
     LUA_OPADD	           = 0;	(* ORDER TM *)
     LUA_OPSUB	           = 1;
     LUA_OPMUL	           = 2;
     LUA_OPDIV	           = 3;
     LUA_OPMOD	           = 4;
     LUA_OPPOW	           = 5;
     LUA_OPUNM	           = 6;

     (* arithmetic comparisons *)
     LUA_OPEQ	           = 0;
     LUA_OPLT	           = 1;
     LUA_OPLE	           = 2;


     (* garbage-collection options *)
     LUA_GCSTOP		   = 0;
     LUA_GCRESTART	   = 1;
     LUA_GCCOLLECT	   = 2;
     LUA_GCCOUNT	   = 3;
     LUA_GCCOUNTB          = 4;
     LUA_GCSTEP		   = 5;
     LUA_GCSETPAUSE	   = 6;
     LUA_GCSETSTEPMUL	   = 7;
     LUA_GCSETMAJORINC	   = 8;
     LUA_GCISRUNNING	   = 9;
     LUA_GCGEN		   = 10;
     LUA_GCINC		   = 11;


     (* event codes *)
     LUA_HOOKCALL          = 0;
     LUA_HOOKRET           = 1;
     LUA_HOOKLINE          = 2;
     LUA_HOOKCOUNT         = 3;
     LUA_HOOKTAILCALL      = 4;

     (* event masks *)
     LUA_MASKCALL          = (1 shl LUA_HOOKCALL);
     LUA_MASKRET           = (1 shl LUA_HOOKRET);
     LUA_MASKLINE          = (1 shl LUA_HOOKLINE);
     LUA_MASKCOUNT         = (1 shl LUA_HOOKCOUNT);


     (* Lua standard library names *)
     LUA_COLIBNAME         = 'coroutine';
     LUA_TABLIBNAME        = 'table';
     LUA_IOLIBNAME         = 'io';
     LUA_OSLIBNAME         = 'os';
     LUA_STRLIBNAME        = 'string';
     LUA_MATHLIBNAME       = 'math';
     LUA_DBLIBNAME         = 'debug';
     LUA_LOADLIBNAME       = 'package';


     LUA_FILEHANDLE        = 'FILE*';
     LUA_IDSIZE	           = 60;
     LUAL_BUFFERSIZE	   = 512;

     (* extra error code for 'luaL_load' *)
     LUA_ERRFILE           = LUA_ERRERR+1;

     (* pre-defined references *)
     LUA_NOREF             = -2;
     LUA_REFNIL            = -1;
     LUA_ENVIRONINDEX      = -10001;
     //LUA_GLOBALSINDEX      = -10002; obsolete in Lua 5.2!!

type
(* TYPES *)
(*============================================================================*)
{$IFDEF WINDOWS}
{$IFDEF CPU64}
     size_t = qword;
{$ENDIF}
{$IFDEF CPU32}
     size_t = cardinal;
{$ENDIF}
     PSize_t = ^size_t;
{$ENDIF}

{$IFDEF FPC}
     TLuaLibHandle = TLibHandle;
{$ELSE}
     {$IFDEF CPU64}
          TLuaLibHandle = qword;
     {$ELSE}
          TLuaLibHandle = cardinal;
     {$ENDIF}
{$ENDIF}

     (* emulation of a C-style char* dynamic array *)
     TCStringDynArray= array of PAnsiChar;
     PCStringDynArray= ^TCStringDynArray;
     (* type of numbers in Lua *)
     lua_Number      = double;
     PLua_Number     = ^lua_Number;
     (* type for integer functions *)
     lua_Integer     = longint;
     PLua_Integer    = ^lua_Integer;
     (* unsigned integer type *)
     lua_Unsigned    = cardinal;
     PLua_Unsigned   = ^lua_Unsigned;

     lua_State       = pointer;
     lua_CFunction   = function (L : lua_State) : integer; cdecl;

     (* functions that read/write blocks when loading/dumping Lua chunks *)
     lua_Reader      = function (L : lua_State; ud : pointer; sz : PSize_t) : PAnsiChar; cdecl;
     lua_Writer      = function (L : lua_State; const p : pointer; sz : size_t; ud : pointer) : integer; cdecl;

     (* prototype for memory-allocation functions *)
     lua_Alloc       = function (ud, ptr : pointer; osize, nsize : size_t) : pointer; cdecl;


     lua_Debug       =
     packed record
            event           : longint;
            name            : PAnsiChar;  (* (n) *)
            namewhat        : PAnsiChar;  (* (n) 'global','local','field','method' *)
            what            : PAnsiChar;  (* (S) 'Lua','C','main','tail' *)
            source          : PAnsiChar;  (* (S) *)
            currentline     : longint;    (* (l) *)
            linedefined     : longint;    (* (S) *)
            lastlinedefined : longint;    (* (S) *)
            nups            : byte;       (* (u) number of upvalues *)
            nparams         : byte;       (* (u) number of parameters *)
            isvararg        : byte;       (* (u) *)
            istailcall      : byte;       (* (t) *)
            short_src       : array [0..LUA_IDSIZE-1] of byte; (* (S) *)
            (* private part *)
            i_ci            : pointer;  (* active function *)
     end;


     luaL_Stream     =
     packed record
            f : pointer;
            closef : lua_CFunction;
     end;


     PLuaL_Buffer = ^luaL_Buffer;
     luaL_Buffer =
     packed record
            b    : PAnsiChar;                            (* buffer address *)
            size : size_t;                                  (* buffer size *)
            n    : size_t;               (* number of characters in buffer *)
            L    : lua_State;
            initb: array [0..LUAL_BUFFERSIZE-1] of ansichar; (* initial buffer *)
     end;


     PLuaL_Reg = ^luaL_Reg;
     luaL_Reg =
     packed record
            name : PAnsiChar;
            func : lua_CFunction;
     end;


var

(* FUNCTIONS *)
(*============================================================================*)

     (* state manipulation *)
     lua_newstate    : function (f : lua_Alloc; ud : pointer) : lua_State; cdecl;
     lua_close       : procedure (L : lua_State); cdecl;
     lua_newthread   : function (L : lua_State) : lua_State; cdecl;
     lua_atpanic     : function (L : lua_State; panicf : lua_CFunction) : lua_CFunction; cdecl;
     lua_version     : function (L : lua_State) : PLua_Number; cdecl;

     (* basic stack manipulation *)
     lua_absindex    : function (L : lua_State; idx : integer) : integer; cdecl;
     lua_gettop      : function (L : lua_State) : integer; cdecl;
     lua_settop      : procedure(L : lua_State; idx : integer); cdecl;
     lua_pushvalue   : procedure(L : lua_State; idx : integer); cdecl;
     lua_remove      : procedure(L : lua_State; idx : integer); cdecl;
     lua_insert      : procedure(L : lua_State; idx : integer); cdecl;
     lua_replace     : procedure(L : lua_State; idx : integer); cdecl;
     lua_copy        : procedure(L : lua_State; fromidx, toidx : integer); cdecl;
     lua_checkstack  : function (L : lua_State; sz : integer) : integer; cdecl;
     lua_xmove       : procedure(from, to_ : lua_State; n : integer); cdecl;

     (* access functions (stack -> C) *)
     lua_isnumber    : function (L : lua_State; idx : integer) : integer; cdecl;
     lua_isstring    : function (L : lua_State; idx : integer) : integer; cdecl;
     lua_iscfunction : function (L : lua_State; idx : integer) : integer; cdecl;
     lua_isuserdata  : function (L : lua_State; idx : integer) : integer; cdecl;
     lua_type        : function (L : lua_State; idx : integer) : integer; cdecl;
     lua_typename    : function (L : lua_State; tp : integer) : PAnsiChar; cdecl;
     lua_tonumberx   : function (L : lua_State; idx : integer; isnum : PInteger) : lua_Number; cdecl;
     lua_tointegerx  : function (L : lua_State; idx : integer; isnum : PInteger) : lua_Integer; cdecl;
     lua_tounsignedx : function (L : lua_State; idx : integer; isnum : PInteger) : lua_Unsigned; cdecl;
     lua_toboolean   : function (L : lua_State; idx : integer) : integer; cdecl;
     lua_tolstring   : function (L : lua_State; idx : integer; len : size_t) : PAnsiChar; cdecl;
     lua_rawlen      : function (L : lua_State; idx : integer) : size_t; cdecl;
     lua_tocfunction : function (L : lua_State; idx : integer) : lua_CFunction; cdecl;
     lua_touserdata  : function (L : lua_State; idx : integer) : pointer; cdecl;
     lua_tothread    : function (L : lua_State; idx : integer) : lua_State; cdecl;
     lua_topointer   : function (L : lua_State; idx : integer) : pointer; cdecl;

     (* arithmetic functions *)
     lua_arith       : procedure(L : lua_State; op : integer); cdecl;
     lua_rawequal    : function (L : lua_State; idx1, idx2 : integer) : integer; cdecl;
     lua_compare     : function (L : lua_State; idx1, idx2, op : integer) : integer; cdecl;

     (* push functions (C -> stack) *)
     lua_pushnil     : procedure(L : lua_State); cdecl;
     lua_pushnumber  : procedure(L : lua_State; n : lua_Number); cdecl;
     lua_pushinteger : procedure(L : lua_State; n : lua_Integer); cdecl;
     lua_pushunsigned: procedure(L : lua_State; n : lua_Unsigned); cdecl;
     lua_pushlstring : function (L : lua_State; s : PAnsiChar; l_ : size_t) : PAnsiChar; cdecl;
     lua_pushstring  : function (L : lua_State; s : PAnsiChar) : PAnsiChar; cdecl;
     lua_pushvfstring: function (L : lua_State; fmt : PAnsiChar; argp : array of const) : PAnsiChar; cdecl;
     lua_pushfstring : function (L : lua_State; fmt : PAnsiChar) : PAnsiChar; cdecl; varargs;
     lua_pushcclosure: procedure(L : lua_State; fn : lua_CFunction; n : integer); cdecl;
     lua_pushboolean : procedure(L : lua_State; b : integer); cdecl;
     lua_pushlightuserdata : procedure(L : lua_State; p : pointer); cdecl;
     lua_pushthread  : function (L : lua_State) : integer; cdecl;

     (* get functions (Lua -> stack) *)
     lua_getglobal   : procedure(L : lua_State; var_ : PAnsiChar); cdecl;
     lua_gettable    : procedure(L : lua_State; idx : integer); cdecl;
     lua_getfield    : procedure(L : lua_State; idx : integer; k : PAnsiChar); cdecl;
     lua_rawget      : procedure(L : lua_State; idx : integer); cdecl;
     lua_rawgeti     : procedure(L : lua_State; idx, n : integer); cdecl;
     lua_rawgetp     : procedure(L : lua_State; idx : integer; p : pointer); cdecl;
     lua_createtable : procedure(L : lua_State; narr, nrec : integer); cdecl;
     lua_newuserdata : function (L : lua_State; sz : size_t) : pointer; cdecl;
     lua_getmetatable: function (L : lua_State; objindex : integer) : integer; cdecl;
     lua_getuservalue: procedure(L : lua_State; idx : integer); cdecl;

     (* set functions (stack -> Lua) *)
     lua_setglobal   : procedure(L : lua_State; var_ : PAnsiChar); cdecl;
     lua_settable    : procedure(L : lua_State; idx : integer); cdecl;
     lua_setfield    : procedure(L : lua_State; idx : integer; k : PAnsiChar); cdecl;
     lua_rawset      : procedure(L : lua_State; idx : integer); cdecl;
     lua_rawseti     : procedure(L : lua_State; idx, n : integer); cdecl;
     lua_rawsetp     : procedure(L : lua_State; idx : integer; p : pointer); cdecl;
     lua_setmetatable: function (L : lua_State; objindex : integer) : integer; cdecl;
     lua_setuservalue: procedure(L : lua_State; idx : integer); cdecl;

     (* 'load' and 'call' functions (load and run Lua code) *)
     lua_call        : procedure(L : lua_State; nargs, nresults : integer; ctx : integer = 0;        //!\\
                                 k : lua_CFunction = nil); cdecl;
     lua_getctx      : function (L : lua_State; ctx : PInteger) : integer; cdecl;
     lua_pcall       : function (L : lua_State; nargs, nresults, errfunc : integer;
                                 ctx : integer = 0; k : lua_CFunction = nil) : integer; cdecl;    //!\\
     lua_load        : function (L : lua_State; reader : lua_Reader; dt : pointer;
                                 chunkname, mode : PAnsiChar) : integer; cdecl;
     lua_dump        : function (L : lua_State; write : lua_Writer; data : pointer) : integer; cdecl;

     (* coroutine functions *)
     lua_yield       : function (L : lua_State; nresults : integer; ctx : integer = 0;
                                 k : lua_CFunction = nil) : integer; cdecl;                //!\\
     lua_resume      : function (L, from : lua_State; narg : integer) : integer; cdecl;
     lua_status      : function (L : lua_State) : integer; cdecl;

     (* garbage collection *)
     lua_gc          : function (L : lua_State; what, data : integer) : integer; cdecl;

     (* miscellaneous functions *)
     lua_error       : function (L : lua_State) : integer; cdecl;
     lua_next        : function (L : lua_State; idx : integer) : integer; cdecl;
     lua_concat      : procedure(L : lua_State; n : integer); cdecl;
     lua_len         : procedure(L : lua_State; idx : integer); cdecl;
     lua_getallocf   : function (L : lua_State; ud : PPointer) : lua_Alloc; cdecl;
     lua_setallocf   : procedure(L : lua_State; f : lua_Alloc; ud : pointer); cdecl;


     (* opening packages *)
     luaopen_base      : function (L : lua_State) : integer; cdecl;
     luaopen_coroutine : function (L : lua_State) : integer; cdecl;
     luaopen_table     : function (L : lua_State) : integer; cdecl;
     luaopen_io        : function (L : lua_State) : integer; cdecl;
     luaopen_os        : function (L : lua_State) : integer; cdecl;
     luaopen_string    : function (L : lua_State) : integer; cdecl;
     luaopen_bit32     : function (L : lua_State) : integer; cdecl;
     luaopen_math      : function (L : lua_State) : integer; cdecl;
     luaopen_debug     : function (L : lua_State) : integer; cdecl;
     luaopen_package   : function (L : lua_State) : integer; cdecl;
     luaL_openlibs     : procedure(L : lua_State); cdecl;
     luaL_pushmodule   : procedure(L : lua_State; modname : PAnsiChar; sizehint : integer); cdecl;
     luaL_openlib      : procedure(L : lua_State; libname : PAnsiChar; lib : PLuaL_Reg;
                                   nup : integer); cdecl;

     (* generic buffer manipulation *)
     luaL_buffinit     : procedure(L : lua_State; B : PLuaL_Buffer); cdecl;
     luaL_prepbuffsize : function (B : PLuaL_Buffer; sz : size_t) : PAnsiChar; cdecl;
     luaL_addlstring   : procedure(B : PLuaL_Buffer; s : PAnsiChar; l : size_t); cdecl;
     luaL_addstring    : procedure(B : PLuaL_Buffer; s : PAnsiChar); cdecl;
     luaL_addvalue     : procedure(B : PLuaL_Buffer); cdecl;
     luaL_pushresult   : procedure(B : PLuaL_Buffer); cdecl;
     luaL_pushresultsize : procedure(B : PLuaL_Buffer; sz : size_t); cdecl;
     luaL_buffinitsize : function (L : lua_State; B : PLuaL_Buffer; sz : size_t) : PAnsiChar; cdecl;

     luaL_checkversion : procedure(L : lua_State; ver : lua_Number = LUA_VERSION_NUM); cdecl;       //!\\
     luaL_getmetafield : function (L : lua_State; obj : integer; e : PAnsiChar) : integer; cdecl;
     luaL_callmeta     : function (L : lua_State; obj : integer; e : PAnsiChar) : integer; cdecl;
     luaL_tolstring    : function (L : lua_State; idx : integer; var len : size_t) : PAnsiChar; cdecl;
     luaL_argerror     : function (L : lua_State; numarg : integer; extramsg : PAnsiChar) : integer; cdecl;
     luaL_checklstring : function (L : lua_State; numArg : integer; var len : size_t) : PAnsiChar; cdecl;
     luaL_optlstring   : function (L : lua_State; numArg : integer; def : PAnsiChar;
                                   len : PSize_t) : PAnsiChar; cdecl;
     luaL_checknumber  : function (L : lua_State; numArg : integer) : lua_Number; cdecl;
     luaL_optnumber    : function (L : lua_State; nArg : integer; def : lua_Number) : lua_Number; cdecl;
     luaL_checkinteger : function (L : lua_State; numArg : integer) : lua_Integer; cdecl;
     luaL_optinteger   : function (L : lua_State; nArg : integer; def : lua_Integer) : lua_Integer; cdecl;
     luaL_checkunsigned: function (L : lua_State; numArg : integer) : lua_Unsigned; cdecl;
     luaL_optunsigned  : function (L : lua_State; numArg : integer; def : lua_Unsigned) : lua_Unsigned; cdecl;

     luaL_checkstack   : procedure(L : lua_State; sz : integer; msg : PAnsiChar); cdecl;
     luaL_checktype    : procedure(L : lua_State; narg, t : integer); cdecl;
     luaL_checkany     : procedure(L : lua_State; narg : integer); cdecl;
     luaL_newmetatable : function (L : lua_State; tname : PAnsiChar) : integer; cdecl;
     luaL_setmetatable : procedure(L : lua_State; tname : PAnsiChar); cdecl;
     luaL_testudata    : function (L : lua_State; ud : integer; tname : PAnsiChar) : pointer; cdecl;
     luaL_checkudata   : function (L : lua_State; ud : integer; tname : PAnsiChar) : pointer; cdecl;

     luaL_where        : procedure(L : lua_State; lvl : integer); cdecl;
     luaL_error        : function (L : lua_State; fmt : PAnsiChar) : integer; varargs; cdecl;

     luaL_checkoption  : function (L : lua_State; narg : integer; def : PAnsiChar;
                                   var lst : array of PAnsiChar) : integer; cdecl;
     luaL_fileresult   : function (L : lua_State; stat : integer; fname : PAnsiChar) : integer; cdecl;
     luaL_execresult   : function (L : lua_State; stat : integer) : integer; cdecl;

     luaL_ref          : function (L : lua_State; t : integer) : integer; cdecl;
     luaL_unref        : procedure(L : lua_State; t, ref : integer); cdecl;

     luaL_loadfile     : function (L : lua_State; filename : PAnsiChar;
                                   mode : PAnsiChar = nil) : integer; cdecl; //!\\
     luaL_loadfilex    : function (L : lua_State; filename : PAnsiChar;
                                   mode : PAnsiChar) : integer; cdecl;

     luaL_loadbuffer   : function (L : lua_State; buff : PAnsiChar; sz : size_t;    //!\\
                                   name : PAnsiChar; mode : PAnsiChar = nil) : integer; cdecl;
     luaL_loadbufferx  : function (L : lua_State; buff : PAnsiChar; sz : size_t;
                                   name : PAnsiChar; mode : PAnsiChar) : integer; cdecl;

     luaL_loadstring   : function (L : lua_State; s : PAnsiChar) : integer; cdecl;
     luaL_newstate     : function : lua_State; cdecl;
     luaL_len          : function (L : lua_State; idx : integer) : integer; cdecl;
     luaL_gsub         : function (L : lua_State; s, p, r : PAnsiChar) : PAnsiChar; cdecl;
     luaL_setfuncs     : procedure(L : lua_State; ld : PLuaL_Reg; nup : integer); cdecl;
     luaL_getsubtable  : function (L : lua_State; idx : integer; fname : PAnsiChar) : integer; cdecl;

     luaL_traceback    : procedure(L, L1 : lua_State; msg : PAnsiChar; level : integer); cdecl;
     luaL_requiref     : procedure(L : lua_State; modname : PAnsiChar; openf : lua_CFunction;
                                   glb : integer); cdecl;


(* MACROS *)
(*============================================================================*)
function lua_upvalueindex(i : integer) : integer; inline;
function lua_tonumber(L : lua_State; i : integer) : lua_Number; inline;
function lua_tointeger(L : lua_State; i : integer) : lua_Integer; inline;
function lua_tounsigned(L : lua_State; i : integer) : lua_Unsigned; inline;

procedure lua_pop(L : lua_State; n : integer); inline;
procedure lua_newtable(L : lua_State); inline;
procedure lua_pushcfunction(L : lua_State; f : lua_CFunction); inline;
procedure lua_register(L : lua_State; n : PAnsiChar; f : lua_CFunction); inline;

function lua_isfunction(L : lua_State; n : integer) : boolean; inline;
function lua_istable(L : lua_State; n : integer) : boolean; inline;
function lua_islightuserdata(L : lua_State; n : integer) : boolean; inline;
function lua_isnil(L : lua_State; n : integer) : boolean; inline;
function lua_isboolean(L : lua_State; n : integer) : boolean; inline;
function lua_isthread(L : lua_State; n : integer) : boolean; inline;
function lua_isnone(L : lua_State; n : integer) : boolean; inline;
function lua_isnoneornil(L : lua_State; n : integer) : boolean; inline;

function lua_pushliteral(L : lua_State; const s : ansistring) : PAnsiChar; inline;
procedure lua_pushglobaltable(L : lua_State); inline;

function lua_tostring(L : lua_State; i : integer) : PAnsiChar; inline;
procedure luaL_register(L : lua_State; n : PAnsiChar; lib : PLuaL_Reg); inline;
function luaL_prepbuffer(B : PLuaL_Buffer) : PAnsiChar; inline;
function luaL_addchar(B : PLuaL_Buffer; c : ansichar) : boolean; inline;
procedure luaL_addsize(B : PLuaL_Buffer; s : size_t); inline;

procedure luaL_newlibtable(L : lua_State; const lib : array of luaL_Reg); inline;
procedure luaL_newlib(L : lua_State; const lib : array of luaL_Reg); inline;
function luaL_argcheck(L : lua_State; cond: boolean; numarg: integer;
                       extramsg: PAnsiChar) : integer; inline;

function luaL_checkstring(L : lua_State; n : integer) : PAnsiChar; inline;
function luaL_optstring(L : lua_State; n : integer; d : PAnsiChar) : PAnsiChar; inline;
function luaL_checkint(L : lua_State; n : integer) : integer; inline;
function luaL_optint(L : lua_State; n : integer; d : lua_Integer) : integer; inline;
function luaL_checklong(L : lua_State; n : integer) : longint; inline;
function luaL_optlong(L : lua_State; n : integer; d : lua_Integer) : longint; inline;
function luaL_typename(L : lua_State; i : integer) : PAnsiChar; inline;
function luaL_dofile(L : lua_State; fn : PAnsiChar) : integer; inline;
function luaL_dostring(L : lua_State; s : PAnsiChar) : integer; inline;
procedure luaL_getmetatable(L : lua_State; n : PAnsiChar); inline;

//function luaL_opt(L : lua_State; f : pointer; n : integer; d : pointer) : integer; inline;

(* DYNAMIC LIBRARY UTILITY FUNCTIONS *)
(*============================================================================*)
function LoadLuaLibrary(const libName : string = '') : boolean;
function FreeLuaLibrary : boolean;
function LastError : PAnsiChar;
(*============================================================================*)


type
    TLuaState = lua_State;

    TLua =
    class(TObject)
         private
               CallbackList : TList;  // internal callback list
               _errorStr    : string;

               function GetError : PChar;
         public
               LuaInstance: TLuaState;  // Lua instance
               constructor Create(AutoRegister: Boolean = True); overload; virtual;
               destructor Destroy; override;

               //simply loads a file and executes it
               function DoFile(const Filename: String; mode : PAnsiChar = nil): Integer; virtual;
               //loads Lua code from a buffer (text or already pre-compiled) and executes it
               function RunFromMem(buffer : PAnsiChar; bufSize : size_t; name : PAnsiChar;
                                   mode : PAnsiChar = nil) : Integer; virtual;
               //registers a function within the interpreter
               procedure RegisterFunction(const FuncName: AnsiString; const mName: AnsiString = '';
                                          Obj: TObject = nil; fptr : pointer = nil); virtual;
{$IFDEF DELPHI}
               // registers all published functions (makes use of Delphi classes)
               procedure AutoRegisterFunctions(Obj: TObject);
{$ENDIF}
               //unregisters all object functions
               procedure UnregisterFunctions(Obj: TObject = nil);

               property Error : PChar read GetError;
    end;

var
   libHandle   : TLuaLibHandle;
   libLoaded   : boolean;
   errorString : string;

implementation

type
  TProc = function(L: TLuaState): Integer of object; // Lua Function

  TCallback =
  class
       Routine: TMethod;  // Code and Data for the method
       Exec: TProc;       // Resulting execution function
  end;


(* MACROS *)
(*============================================================================*)
function lua_upvalueindex(i : integer) : integer; inline;
begin
     Result := LUA_REGISTRYINDEX - i;
end;


function lua_tonumber(L : lua_State; i : integer) : lua_Number; inline;
begin
     Result := lua_tonumberx(L,i,nil);
end;


function lua_tointeger(L : lua_State; i : integer) : lua_Integer; inline;
begin
     Result := lua_tointegerx(L,i,nil);
end;


function lua_tounsigned(L : lua_State; i : integer) : lua_Unsigned; inline;
begin
     Result := lua_tounsignedx(L,i,nil);
end;


procedure lua_pop(L : lua_State; n : integer); inline;
begin
     lua_settop(L,-(n)-1);
end;


procedure lua_newtable(L : lua_State); inline;
begin
     lua_createtable(L,0,0);
end;


procedure lua_pushcfunction(L : lua_State; f : lua_CFunction); inline;
begin
     lua_pushcclosure(L,f,0);
end;


procedure lua_register(L : lua_State; n : PAnsiChar; f : lua_CFunction); inline;
begin
     lua_pushcfunction(L,f);
     lua_setglobal(L, n);
end;


function lua_isfunction(L : lua_State; n : integer) : boolean; inline;
begin
     Result := (lua_type(L,n) = LUA_TFUNCTION);
end;


function lua_istable(L : lua_State; n : integer) : boolean; inline;
begin
     Result := (lua_type(L,n) = LUA_TTABLE);
end;


function lua_islightuserdata(L : lua_State; n : integer) : boolean; inline;
begin
     Result := (lua_type(L,n) = LUA_TLIGHTUSERDATA);
end;


function lua_isnil(L : lua_State; n : integer) : boolean; inline;
begin
     Result := (lua_type(L,n) = LUA_TNIL);
end;


function lua_isboolean(L : lua_State; n : integer) : boolean; inline;
begin
     Result := (lua_type(L,n) = LUA_TBOOLEAN);
end;


function lua_isthread(L : lua_State; n : integer) : boolean; inline;
begin
     Result := (lua_type(L,n) = LUA_TTHREAD);
end;


function lua_isnone(L : lua_State; n : integer) : boolean; inline;
begin
     Result := (lua_type(L,n) = LUA_TNONE);
end;


function lua_isnoneornil(L : lua_State; n : integer) : boolean; inline;
begin
     Result := (lua_type(L,n) <= 0);
end;


function lua_pushliteral(L : lua_State; const s : ansistring) : PAnsiChar; inline;
begin
     Result := lua_pushlstring(L, PAnsiChar(s), length(s));
end;


procedure lua_pushglobaltable(L : lua_State); inline;
begin
     lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS);
end;


function lua_tostring(L : lua_State; i : integer) : PAnsiChar; inline;
begin
     Result := lua_tolstring(L, i, 0);
end;


procedure luaL_register(L : lua_State; n : PAnsiChar; lib : PLuaL_Reg); inline;
begin
     luaL_openlib(L,n,lib,0);
end;


function luaL_prepbuffer(B : PLuaL_Buffer) : PAnsiChar; inline;
begin
     Result := luaL_prepbuffsize(B, LUAL_BUFFERSIZE);
end;


function luaL_addchar(B : PLuaL_Buffer; c : ansichar) : boolean; inline;
begin
     //SIZE_T are used as containers to accomodate both 32 and 64-bit builds
     ////!\\ this will not work on OSes > 64-bit
     Result := size_t(B^.b) < size_t(B^.initb + LUAL_BUFFERSIZE);
     if Result then
          luaL_prepbuffer(B);

     B^.b^ := c;
     Inc(B^.b);

(*     Result := ByteBool((B^).n < (B^).size or ord(ansichar(luaL_prepbuffsize(B,1))));
     (B^).b[(B^).n] := c;
     (B^).n := (B^).n + 1;*)
end;


procedure luaL_addsize(B : PLuaL_Buffer; s : size_t); inline;
begin
     (B^).n := (B^).n + s;
end;


procedure luaL_newlibtable(L : lua_State; const lib : array of luaL_Reg); inline;
begin
     lua_createtable(L, 0, length(lib));
end;


procedure luaL_newlib(L : lua_State; const lib : array of luaL_Reg); inline;
begin
     lua_createtable(L, 0, length(lib));
     luaL_setfuncs(L,@lib[0],0);
end;


function luaL_argcheck(L : lua_State; cond: boolean; numarg: integer;
                       extramsg: PAnsiChar) : integer; inline;
begin
     if cond then
         Result := 0
     else
         Result := luaL_argerror(L, numarg, extramsg);
end;


function luaL_checkstring(L : lua_State; n : integer) : PAnsiChar; inline;
var len : size_t;
begin
     len    := 0;
     Result := luaL_checklstring(L, n, len);
end;


function luaL_optstring(L : lua_State; n : integer; d : PAnsiChar) : PAnsiChar; inline;
begin
     Result := luaL_optlstring(L, n, d, nil);
end;


function luaL_checkint(L : lua_State; n : integer) : integer; inline;
begin
     Result := integer(luaL_checkinteger(L,n));
end;


function luaL_optint(L : lua_State; n : integer; d : lua_Integer) : integer; inline;
begin
     Result := integer(luaL_optinteger(L,n,d));
end;


function luaL_checklong(L : lua_State; n : integer) : longint; inline;
begin
     Result := longint(luaL_checkinteger(L,n));
end;


function luaL_optlong(L : lua_State; n : integer; d : lua_Integer) : longint; inline;
begin
     Result := longint(luaL_optinteger(L,n,d));
end;


function luaL_typename(L : lua_State; i : integer) : PAnsiChar; inline;
begin
     Result := lua_typename(L, lua_type(L,i));
end;


function luaL_dofile(L : lua_State; fn : PAnsiChar) : integer; inline;
begin
     Result := luaL_loadfile(L,fn) or lua_pcall(L,0,LUA_MULTRET,0);
end;


function luaL_dostring(L : lua_State; s : PAnsiChar) : integer; inline;
begin
     Result := luaL_loadstring(L, s) or lua_pcall(L, 0, LUA_MULTRET, 0);
end;


procedure luaL_getmetatable(L : lua_State; n : PAnsiChar); inline;
begin
     lua_getfield(L, LUA_REGISTRYINDEX, n);
end;


//
// This function is called by Lua, it extracts the object by
// pointer to the objects method by name, which is then called.
//
// @param       Lua_State   L   Pointer to Lua instance
// @return      Integer         Number of result arguments on stack
//
function LuaCallBack(L: Lua_State): Integer; cdecl;
var
   CallBack: TCallBack;       // The Object stored in the Object Table
   ptr     : pointer;
begin
     ptr := lua_touserdata(L, lua_upvalueindex(1));
     // Retrieve first Closure Value (=Object Pointer)
     CallBack := TCallBack(ptr);

     // Execute only if Object is valid
     if (assigned(CallBack) and assigned(CallBack.Exec)) then
         Result := CallBack.Exec(L)
     else
         Result := 0;
end;

{ TLua }

function TLua.GetError : PChar;
begin
     Result := PChar(_errorStr);
end;


//
// Create a new Lua instance and optionally create Lua functions
//
// @param       Boolean      AutoRegister       (optional)
// @return      TLua                            Lua Instance
//
constructor TLua.Create(AutoRegister: Boolean = True);
begin
     inherited Create;
     // Load Lua Lib if not already done
     _errorStr := '';

     if (not libLoaded) then
     begin
          if not LoadLuaLibrary then
               _errorStr := errorString;
     end;

     try
       // Open Library
       LuaInstance := luaL_newstate;
       luaopen_base(LuaInstance);

       // Create Object List on initialization
       CallBackList := TList.Create;

       // if set then register published functions
{$IFDEF DELPHI}
       if (AutoRegister) then
           AutoRegisterFunctions(self);
{$ENDIF}

     except
       on E:Exception do _errorStr := 'Error while creating TLua class : '+E.Message;
     end;
end;

//
// Dispose Lua instance
//
destructor TLua.Destroy;
begin
     // Unregister all functions
     UnregisterFunctions(Self);

     // dispose Object List on finalization
     CallBackList.Free;

     // Close instance
     lua_close(LuaInstance);

     inherited;
end;

//
// Wrapper for Lua File load and Execution
//
// @param       String  Filename        Lua Script file name
// @param       PAnsiChar mode          (optional) Lua Script type accepted : binary "b", text "t", both "bt" (default)
// @return      Integer
//
function TLua.DoFile(const Filename: String; mode : PAnsiChar = nil) : Integer;
begin
     try
       Result := luaL_loadfile(LuaInstance,PAnsiChar(AnsiString(Filename)),mode);
       if Result <> 0 then
            raise Exception.Create('Cannot load file '+Filename+'.');

       Result := lua_pcall(LuaInstance, 0, LUA_MULTRET, 0);
       case Result of
            LUA_ERRRUN    : raise Exception.Create('Runtime error in "'+Filename+'"');
            LUA_ERRMEM    : raise Exception.Create('Memory allocation error in "'+Filename+'"');
            LUA_ERRSYNTAX : raise Exception.Create('Syntax error in "'+Filename+'"');
            LUA_ERRERR    : raise Exception.Create('Error handling function failed in "'+Filename+'"');
       end;

     except
       on E:Exception do _errorStr := E.Message + ' : ' + lua_tostring(LuaInstance, -1);
     end;
end;


//
// Wrapper for Lua Buffer load and Execution
//
// @param       PAnsiChar buffer   Lua Script string
// @param       size_t    bufSize  Lua Script string size, in bytes
// @param       PAnsiChar name     Lua Script name, for debug and error reporting purposes
// @param       PAnsiChar mode     (optional) Lua Script type accepted : binary "b", text "t", both "bt" (default)
// @return      Integer
//
function TLua.RunFromMem(buffer : PAnsiChar; bufSize : size_t; name : PAnsiChar; mode : PAnsiChar = nil) : Integer;
begin
     try
       Result := luaL_loadbuffer(LuaInstance,buffer,bufSize,name,mode);
       if Result <> 0 then
            raise Exception.Create('Cannot load buffer for '+name+'.');

       Result := lua_pcall(LuaInstance, 0, LUA_MULTRET, 0);
       case Result of
            LUA_ERRRUN    : raise Exception.Create('Runtime error in "'+name+'"');
            LUA_ERRMEM    : raise Exception.Create('Memory allocation error in "'+name+'"');
            LUA_ERRSYNTAX : raise Exception.Create('Syntax error in "'+name+'"');
            LUA_ERRERR    : raise Exception.Create('Error handling function failed in "'+name+'"');
       end;

     except
       on E:Exception do _errorStr := E.Message + ' : ' + lua_tostring(LuaInstance, -1);
     end;
end;


//
// Register a new Lua Function and map it to the Objects methoif (assigned(CallBack) and assigned(CallBack.Exec)) thend name
//
// @param       AnsiString      FuncName        Lua Function Name
// @param       AnsiString      MethodName      (optional) Objects Method name
// @param       TObject         Obj             (optional) Object pointer to provide instance context at function call
// @param       pointer         fptr            (optional) pointer to the function, in case we want to pass a
//                                              non-method function (Obj must be nil)
//
procedure TLua.RegisterFunction(const FuncName: AnsiString; const mName: AnsiString = ''; Obj: TObject = nil; fptr : pointer = nil);
var
   CallBack: TCallBack; // Callback Object
begin
     // Add Callback Object to the Object Index
     CallBack := TCallBack.Create;

     // if not object specified use this object
     if (Obj = nil) then
     begin
          CallBack.Routine.Data := LuaInstance;
          CallBack.Routine.Code := fptr;
     end
     else
     begin
          CallBack.Routine.Data := Obj;

          // if method name not specified use Lua function name
          if (mName = '') then
              CallBack.Routine.Code := Obj.MethodAddress(FuncName)
          else
              CallBack.Routine.Code := Obj.MethodAddress(mName);
     end;

     CallBack.Exec := TProc(CallBack.Routine);
     CallbackList.Add(CallBack);

     // prepare Closure value (CallBack Object Pointer)
     lua_pushlightuserdata(LuaInstance, pointer(CallBack));

     // set new Lua function with Closure value
     lua_pushcclosure(LuaInstance, @LuaCallBack, 1);

     // set table using the method's name
     lua_setglobal(LuaInstance,PAnsiChar(FuncName));
end;

//
// UnRegister all new Lua Function
//
// @param       TObject     Object      Object with prev registered lua functions
//
procedure TLua.UnregisterFunctions(Obj: TObject = nil);
var
   i: Integer;
   CallBack: TCallBack;
begin
     // remove obj from object list
     for i := CallBackList.Count downto 1 do
     begin
          CallBack := TCallBack(CallBackList[i-1]);
          if (assigned(CallBack)) then
          begin
               if ((Obj <> nil) and (CallBack.Routine.Data = @Obj)) or
                  ((Obj = nil) and (CallBack.Routine.Data = LuaInstance)) then
               begin
                    FreeAndNil(CallBack);
                    CallBackList.Items[i-1] := nil;
                    CallBackList.Delete(i-1);
               end;
          end;
     end;
end;

//
// Register all published methods as Lua Functions
//
// //!\\ May not work with Freepascal at all (different class structure)
//Use only if working with Delphi classes
{$IFDEF DELPHI}
procedure TLua.AutoRegisterFunctions(Obj: TObject);
type
    PPointer = ^Pointer;
    PMethodRec = ^TMethodRec;

    TMethodRec =
    packed record
           wSize: Word;
           pCode: Pointer;
           sName: ShortString;
    end;
var
   MethodTable: PAnsiChar;
   MethodRec: PMethodRec;
   wCount: Word;
   nMethod: Integer;
begin
     // Get a pointer to the class's published method table
     MethodTable := PAnsiChar(Pointer(PAnsiChar(Obj.ClassType) + vmtMethodTable)^);

     if (MethodTable <> Nil) then
     begin
          // Get the count of the methods in the table
          Move(MethodTable^, wCount, 2);

          // Position the MethodRec pointer at the first method in the table
          // (skip over the 2-byte method count)
          MethodRec := PMethodRec(MethodTable + 2);

          // Iterate through all the published methods of this class
          for nMethod := 0 to wCount - 1 do
          begin
               // Add the method name to the lua functions
               RegisterFunction(MethodRec^.sName, MethodRec^.sName, Obj);
               // Skip to the next method
               MethodRec := PMethodRec(cardinal(PAnsiChar(MethodRec)) + MethodRec^.wSize);
          end;
     end;
end;
{$ENDIF}

//no try-except blocks in order to prune the SysUtils unit from uses clause
function LoadLuaLibrary(const libName : string = '') : boolean;
begin
     Result      := false;
     errorString := '';
     libLoaded   := false;

     if libName <> '' then
         libHandle := LoadLibrary(libName)
     else
         libHandle := LoadLibrary(LUA_API);

     if libHandle = 0 then
     begin
          if libName <> '' then
              errorString := 'could not load Lua library "'+libName+'".'
          else
              errorString := 'could not load Lua library "'+LUA_API+'".';

          exit(false);
     end;

     lua_newstate            := GetProcAddress(libHandle, 'lua_newstate');
     lua_close               := GetProcAddress(libHandle, 'lua_close');
     lua_newthread           := GetProcAddress(libHandle, 'lua_newthread');
     lua_atpanic             := GetProcAddress(libHandle, 'lua_atpanic');
     lua_version             := GetProcAddress(libHandle, 'lua_version');

     (* basic stack manipulation *)
     lua_absindex            := GetProcAddress(libHandle, 'lua_absindex');
     lua_gettop              := GetProcAddress(libHandle, 'lua_gettop');
     lua_settop              := GetProcAddress(libHandle, 'lua_settop');
     lua_pushvalue           := GetProcAddress(libHandle, 'lua_pushvalue');
     lua_remove              := GetProcAddress(libHandle, 'lua_remove');
     lua_insert              := GetProcAddress(libHandle, 'lua_insert');
     lua_replace             := GetProcAddress(libHandle, 'lua_replace');
     lua_copy                := GetProcAddress(libHandle, 'lua_copy');
     lua_checkstack          := GetProcAddress(libHandle, 'lua_checkstack');
     lua_xmove               := GetProcAddress(libHandle, 'lua_xmove');

     (* access functions (stack -> C) *)
     lua_isnumber            := GetProcAddress(libHandle, 'lua_isnumber');
     lua_isstring            := GetProcAddress(libHandle, 'lua_isstring');
     lua_iscfunction         := GetProcAddress(libHandle, 'lua_iscfunction');
     lua_isuserdata          := GetProcAddress(libHandle, 'lua_isuserdata');
     lua_type                := GetProcAddress(libHandle, 'lua_type');
     lua_typename            := GetProcAddress(libHandle, 'lua_typename');
     lua_tonumberx           := GetProcAddress(libHandle, 'lua_tonumberx');
     lua_tointegerx          := GetProcAddress(libHandle, 'lua_tointegerx');
     lua_tounsignedx         := GetProcAddress(libHandle, 'lua_tounsignedx');
     lua_toboolean           := GetProcAddress(libHandle, 'lua_toboolean');
     lua_tolstring           := GetProcAddress(libHandle, 'lua_tolstring');
     lua_rawlen              := GetProcAddress(libHandle, 'lua_rawlen');
     lua_tocfunction         := GetProcAddress(libHandle, 'lua_tocfunction');
     lua_touserdata          := GetProcAddress(libHandle, 'lua_touserdata');
     lua_tothread            := GetProcAddress(libHandle, 'lua_tothread');
     lua_topointer           := GetProcAddress(libHandle, 'lua_topointer');

     (* arithmetic functions *)
     lua_arith               := GetProcAddress(libHandle, 'lua_arith');
     lua_rawequal            := GetProcAddress(libHandle, 'lua_rawequal');
     lua_compare             := GetProcAddress(libHandle, 'lua_compare');

     (* push functions (C -> stack) *)
     lua_pushnil             := GetProcAddress(libHandle, 'lua_pushnil');
     lua_pushnumber          := GetProcAddress(libHandle, 'lua_pushnumber');
     lua_pushinteger         := GetProcAddress(libHandle, 'lua_pushinteger');
     lua_pushunsigned        := GetProcAddress(libHandle, 'lua_pushunsigned');
     lua_pushlstring         := GetProcAddress(libHandle, 'lua_pushlstring');
     lua_pushstring          := GetProcAddress(libHandle, 'lua_pushstring');
     lua_pushvfstring        := GetProcAddress(libHandle, 'lua_pushvfstring');
     lua_pushfstring         := GetProcAddress(libHandle, 'lua_pushfstring');
     lua_pushcclosure        := GetProcAddress(libHandle, 'lua_pushcclosure');
     lua_pushboolean         := GetProcAddress(libHandle, 'lua_pushboolean');
     lua_pushlightuserdata   := GetProcAddress(libHandle, 'lua_pushlightuserdata');
     lua_pushthread          := GetProcAddress(libHandle, 'lua_pushthread');

     (* get functions (Lua -> stack) *)
     lua_getglobal           := GetProcAddress(libHandle, 'lua_getglobal');
     lua_gettable            := GetProcAddress(libHandle, 'lua_gettable');
     lua_getfield            := GetProcAddress(libHandle, 'lua_getfield');
     lua_rawget              := GetProcAddress(libHandle, 'lua_rawget');
     lua_rawgeti             := GetProcAddress(libHandle, 'lua_rawgeti');
     lua_rawgetp             := GetProcAddress(libHandle, 'lua_rawgetp');
     lua_createtable         := GetProcAddress(libHandle, 'lua_createtable');
     lua_newuserdata         := GetProcAddress(libHandle, 'lua_newuserdata');
     lua_getmetatable        := GetProcAddress(libHandle, 'lua_getmetatable');
     lua_getuservalue        := GetProcAddress(libHandle, 'lua_getuservalue');

     (* set functions (stack -> Lua) *)
     lua_setglobal           := GetProcAddress(libHandle, 'lua_setglobal');
     lua_settable            := GetProcAddress(libHandle, 'lua_settable');
     lua_setfield            := GetProcAddress(libHandle, 'lua_setfield');
     lua_rawset              := GetProcAddress(libHandle, 'lua_rawset');
     lua_rawseti             := GetProcAddress(libHandle, 'lua_rawseti');
     lua_rawsetp             := GetProcAddress(libHandle, 'lua_rawsetp');
     lua_setmetatable        := GetProcAddress(libHandle, 'lua_setmetatable');
     lua_setuservalue        := GetProcAddress(libHandle, 'lua_setuservalue');

     (* 'load' and 'call' functions (load and run Lua code) *)
     lua_call                := GetProcAddress(libHandle, 'lua_callk');
     lua_getctx              := GetProcAddress(libHandle, 'lua_getctx');
     lua_pcall               := GetProcAddress(libHandle, 'lua_pcallk');
     lua_load                := GetProcAddress(libHandle, 'lua_load');
     lua_dump                := GetProcAddress(libHandle, 'lua_dump');

     (* coroutine functions *)
     lua_yield               := GetProcAddress(libHandle, 'lua_yieldk');
     lua_resume              := GetProcAddress(libHandle, 'lua_resume');
     lua_status              := GetProcAddress(libHandle, 'lua_status');

     (* garbage collection *)
     lua_gc                  := GetProcAddress(libHandle, 'lua_gc');

     (* miscellaneous functions *)
     lua_error               := GetProcAddress(libHandle, 'lua_error');
     lua_next                := GetProcAddress(libHandle, 'lua_next');
     lua_concat              := GetProcAddress(libHandle, 'lua_concat');
     lua_len                 := GetProcAddress(libHandle, 'lua_len');
     lua_getallocf           := GetProcAddress(libHandle, 'lua_getallocf');
     lua_setallocf           := GetProcAddress(libHandle, 'lua_setallocf');


     (* opening packages *)
     luaopen_base            := GetProcAddress(libHandle, 'luaopen_base');
     luaopen_coroutine       := GetProcAddress(libHandle, 'luaopen_coroutine');
     luaopen_table           := GetProcAddress(libHandle, 'luaopen_table');
     luaopen_io              := GetProcAddress(libHandle, 'luaopen_io');
     luaopen_os              := GetProcAddress(libHandle, 'luaopen_os');
     luaopen_string          := GetProcAddress(libHandle, 'luaopen_string');
     luaopen_bit32           := GetProcAddress(libHandle, 'luaopen_bit32');
     luaopen_math            := GetProcAddress(libHandle, 'luaopen_math');
     luaopen_debug           := GetProcAddress(libHandle, 'luaopen_debug');
     luaopen_package         := GetProcAddress(libHandle, 'luaopen_package');
     luaL_openlibs           := GetProcAddress(libHandle, 'luaL_openlibs');
     luaL_pushmodule         := GetProcAddress(libHandle, 'luaL_pushmodule');
     luaL_openlib            := GetProcAddress(libHandle, 'luaL_openlib');

     (* generic buffer manipulation *)
     luaL_buffinit           := GetProcAddress(libHandle, 'luaL_buffinit');
     luaL_prepbuffsize       := GetProcAddress(libHandle, 'luaL_prepbuffsize');
     luaL_addlstring         := GetProcAddress(libHandle, 'luaL_addlstring');
     luaL_addstring          := GetProcAddress(libHandle, 'luaL_addstring');
     luaL_addvalue           := GetProcAddress(libHandle, 'luaL_addvalue');
     luaL_pushresult         := GetProcAddress(libHandle, 'luaL_pushresult');
     luaL_pushresultsize     := GetProcAddress(libHandle, 'luaL_pushresultsize');
     luaL_buffinitsize       := GetProcAddress(libHandle, 'luaL_buffinitsize');

     luaL_checkversion       := GetProcAddress(libHandle, 'luaL_checkversion_');
     luaL_getmetafield       := GetProcAddress(libHandle, 'luaL_getmetafield');
     luaL_callmeta           := GetProcAddress(libHandle, 'luaL_callmeta');
     luaL_tolstring          := GetProcAddress(libHandle, 'luaL_tolstring');
     luaL_argerror           := GetProcAddress(libHandle, 'luaL_argerror');
     luaL_checklstring       := GetProcAddress(libHandle, 'luaL_checklstring');
     luaL_optlstring         := GetProcAddress(libHandle, 'luaL_optlstring');
     luaL_checknumber        := GetProcAddress(libHandle, 'luaL_checknumber');
     luaL_optnumber          := GetProcAddress(libHandle, 'luaL_optnumber');
     luaL_checkinteger       := GetProcAddress(libHandle, 'luaL_checkinteger');
     luaL_optinteger         := GetProcAddress(libHandle, 'luaL_optinteger');
     luaL_checkunsigned      := GetProcAddress(libHandle, 'luaL_checkunsigned');
     luaL_optunsigned        := GetProcAddress(libHandle, 'luaL_optunsigned');

     luaL_checkstack         := GetProcAddress(libHandle, 'luaL_checkstack');
     luaL_checktype          := GetProcAddress(libHandle, 'luaL_checktype');
     luaL_checkany           := GetProcAddress(libHandle, 'luaL_checkany');
     luaL_newmetatable       := GetProcAddress(libHandle, 'luaL_newmetatable');
     luaL_setmetatable       := GetProcAddress(libHandle, 'luaL_setmetatable');
     luaL_testudata          := GetProcAddress(libHandle, 'luaL_testudata');
     luaL_checkudata         := GetProcAddress(libHandle, 'luaL_checkudata');

     luaL_where              := GetProcAddress(libHandle, 'luaL_where');
     luaL_error              := GetProcAddress(libHandle, 'luaL_error');

     luaL_checkoption        := GetProcAddress(libHandle, 'luaL_checkoption');
     luaL_fileresult         := GetProcAddress(libHandle, 'luaL_fileresult');
     luaL_execresult         := GetProcAddress(libHandle, 'luaL_execresult');

     luaL_ref                := GetProcAddress(libHandle, 'luaL_ref');
     luaL_unref              := GetProcAddress(libHandle, 'luaL_unref');

     luaL_loadfile           := GetProcAddress(libHandle, 'luaL_loadfilex');
     luaL_loadfilex          := luaL_loadfile;

     luaL_loadbuffer         := GetProcAddress(libHandle, 'luaL_loadbufferx');
     luaL_loadbufferx        := luaL_loadbuffer;

     luaL_loadstring         := GetProcAddress(libHandle, 'luaL_loadstring');
     luaL_newstate           := GetProcAddress(libHandle, 'luaL_newstate');
     luaL_len                := GetProcAddress(libHandle, 'luaL_len');
     luaL_gsub               := GetProcAddress(libHandle, 'luaL_gsub');
     luaL_setfuncs           := GetProcAddress(libHandle, 'luaL_setfuncs');
     luaL_getsubtable        := GetProcAddress(libHandle, 'luaL_getsubtable');

     luaL_traceback          := GetProcAddress(libHandle, 'luaL_traceback');
     luaL_requiref           := GetProcAddress(libHandle, 'luaL_requiref');

     Result := true;
     libLoaded := true;
end;


function FreeLuaLibrary : boolean;
begin
     lua_newstate            := nil;
     lua_close               := nil;
     lua_newthread           := nil;
     lua_atpanic             := nil;
     lua_version             := nil;
     lua_absindex            := nil;
     lua_gettop              := nil;
     lua_settop              := nil;
     lua_pushvalue           := nil;
     lua_remove              := nil;
     lua_insert              := nil;
     lua_replace             := nil;
     lua_copy                := nil;
     lua_checkstack          := nil;
     lua_xmove               := nil;
     lua_isnumber            := nil;
     lua_isstring            := nil;
     lua_iscfunction         := nil;
     lua_isuserdata          := nil;
     lua_type                := nil;
     lua_typename            := nil;
     lua_tonumberx           := nil;
     lua_tointegerx          := nil;
     lua_tounsignedx         := nil;
     lua_toboolean           := nil;
     lua_tolstring           := nil;
     lua_rawlen              := nil;
     lua_tocfunction         := nil;
     lua_touserdata          := nil;
     lua_tothread            := nil;
     lua_topointer           := nil;
     lua_arith               := nil;
     lua_rawequal            := nil;
     lua_compare             := nil;
     lua_pushnil             := nil;
     lua_pushnumber          := nil;
     lua_pushinteger         := nil;
     lua_pushunsigned        := nil;
     lua_pushlstring         := nil;
     lua_pushstring          := nil;
     lua_pushvfstring        := nil;
     lua_pushfstring         := nil;
     lua_pushcclosure        := nil;
     lua_pushboolean         := nil;
     lua_pushlightuserdata   := nil;
     lua_pushthread          := nil;
     lua_getglobal           := nil;
     lua_gettable            := nil;
     lua_getfield            := nil;
     lua_rawget              := nil;
     lua_rawgeti             := nil;
     lua_rawgetp             := nil;
     lua_createtable         := nil;
     lua_newuserdata         := nil;
     lua_getmetatable        := nil;
     lua_getuservalue        := nil;
     lua_setglobal           := nil;
     lua_settable            := nil;
     lua_setfield            := nil;
     lua_rawset              := nil;
     lua_rawseti             := nil;
     lua_rawsetp             := nil;
     lua_setmetatable        := nil;
     lua_setuservalue        := nil;
     lua_call                := nil;
     lua_getctx              := nil;
     lua_pcall               := nil;
     lua_load                := nil;
     lua_dump                := nil;
     lua_yield               := nil;
     lua_resume              := nil;
     lua_status              := nil;
     lua_gc                  := nil;
     lua_error               := nil;
     lua_next                := nil;
     lua_concat              := nil;
     lua_len                 := nil;
     lua_getallocf           := nil;
     lua_setallocf           := nil;
     luaopen_base            := nil;
     luaopen_coroutine       := nil;
     luaopen_table           := nil;
     luaopen_io              := nil;
     luaopen_os              := nil;
     luaopen_string          := nil;
     luaopen_bit32           := nil;
     luaopen_math            := nil;
     luaopen_debug           := nil;
     luaopen_package         := nil;
     luaL_openlibs           := nil;
     luaL_pushmodule         := nil;
     luaL_openlib            := nil;
     luaL_buffinit           := nil;
     luaL_prepbuffsize       := nil;
     luaL_addlstring         := nil;
     luaL_addstring          := nil;
     luaL_addvalue           := nil;
     luaL_pushresult         := nil;
     luaL_pushresultsize     := nil;
     luaL_buffinitsize       := nil;
     luaL_checkversion       := nil;
     luaL_getmetafield       := nil;
     luaL_callmeta           := nil;
     luaL_tolstring          := nil;
     luaL_argerror           := nil;
     luaL_checklstring       := nil;
     luaL_optlstring         := nil;
     luaL_checknumber        := nil;
     luaL_optnumber          := nil;
     luaL_checkinteger       := nil;
     luaL_optinteger         := nil;
     luaL_checkunsigned      := nil;
     luaL_optunsigned        := nil;
     luaL_checkstack         := nil;
     luaL_checktype          := nil;
     luaL_checkany           := nil;
     luaL_newmetatable       := nil;
     luaL_setmetatable       := nil;
     luaL_testudata          := nil;
     luaL_checkudata         := nil;
     luaL_where              := nil;
     luaL_error              := nil;
     luaL_checkoption        := nil;
     luaL_fileresult         := nil;
     luaL_execresult         := nil;
     luaL_ref                := nil;
     luaL_unref              := nil;
     luaL_loadfile           := nil;
     luaL_loadfilex          := nil;
     luaL_loadbuffer         := nil;
     luaL_loadbufferx        := nil;
     luaL_loadstring         := nil;
     luaL_newstate           := nil;
     luaL_len                := nil;
     luaL_gsub               := nil;
     luaL_setfuncs           := nil;
     luaL_getsubtable        := nil;
     luaL_traceback          := nil;
     luaL_requiref           := nil;

     Result    := false;
     libLoaded := false;

     if libHandle <> 0 then
     begin
          FreeLibrary(libHandle);
          Result := true;
     end;

end;


function LastError : PAnsiChar;
begin
     Result := PAnsiChar(errorString);
end;


end.

