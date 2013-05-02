%% @author Raimar Falke
%% @copyright 2013 Raimar Falke
% Distrbuted under GNU General Public License version 2

-record(options,{seedurl=[], 
		 basedir=".", 
		 recurse=none, 
		 acceptUrlTest=unset,
		 includePattern=",,",
		 parallel=1}).
