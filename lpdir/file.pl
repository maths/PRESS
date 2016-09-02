%   File   : FILE
%   Author : Bernard (following Lincoln)
%   Updated: 8 August 1985
%   Purpose:  Code for checking File Names for UNIX validity

% Note: More restrictive than UNIX, alphanumerics only in filenames.


	% valid_filename(+Filename) is true if 
	% 
	% 	Filename denotes a file specification of the form:
	% 	========
	% 		   specification --> (path)filename.(ext)
	%	
	%		   path --> abspath | relpath | tildepath
	%
	%		   abspath --> /relpath
	%
	%		   relpath --> pathname(/relpath)
	%		   
	%		   tildepath --> ~abspath | ~relpath
	%
	%		   pathname --> > 1 alphanumeric characters
	%	
	%		   filename --> < 11 alphanumeric characters
	%	
	%		   ext --> < 4 alphanumeric characters
	%	
	% 		 The path and extension are optional.  
	%		 


valid_filename(FileName) :-
	name(FileName,Chars),
	valid_filename(Chars,[]).

valid_filename --> path,filename.
valid_filename --> filename.

path --> abspath | relpath | tildepath.
abspath --> slash,relpath.
tildepath --> tilde,slash,aftertilde.
tildepath --> tilde,username,slash,aftertilde.
aftertilde --> relpath,filename.
aftertilde --> filename.
relpath --> pathname,slash.
relpath --> pathname,slash,relpath.
filename --> file,dot,extension.
filename --> file.

file --> alphanumeric_string(11,0,_).
extension -->
	alphanumeric_string(3,0,L),{L>0}.
extension --> "".
username --> alphanumeric_string(11,0,_).
pathname --> 
	alphanumeric_string(14,0,L),{L>0}.

alphanumeric_string --> alphanumeric_char,alphanumeric_string.
alphanumeric_string --> alphanumeric_char.


alphanumeric_string(Bound, N, StringLength) -->
	{N < Bound},
	alphanumeric_char,
	{M is N+1}, !,
	alphanumeric_string(Bound, M, StringLength).
alphanumeric_string(_,N,N) --> "".


	alphanumeric_char --> [X], {"a" =< X, X =< "z"}, !.
	alphanumeric_char --> [X], {"A" =< X, X =< "Z"}, !.
	alphanumeric_char --> [X], {"0" =< X, X =< "9"}.


slash --> "/".

dot --> ".".

tilde --> "~".
