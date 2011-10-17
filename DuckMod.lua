--[[
	DuckMod - A World of Warcraft add-on library
	Copyright (C) 2009  Dag Bakken

	DuckMod is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	DuckMod is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with DuckMod.  If not, see <http://www.gnu.org/licenses/>.
]]

TestDebugTable={}

local TV=2.07;
-- 2.07
-- + WoWnet v0.01: Data transmission by Base64 coding for full digital
--   transmission. Supports multiple page reception.
-- o WN Renderer
-- 2.06
-- + Work on WoWnet progresses. Finally.
-- + Added simple generic multi-threading library methods
-- + Fixed bug in cache that effectively switched it off
-- 2.04
-- + Changed cache to only work for decoded base data (who), enabling
--   decode of many sources without corrupting the cache.
-- + Added an extra level for cache tables
-- 2.0204
-- + Detects uncoded text in "Decode"
-- + Added "Purge" for table-cache
-- 2.0203
-- + Fixed showstopper bug in event handling
-- + Fixed bug in password and coding
-- 2.0202
-- + Table compression enhancement
-- 2.0201
-- + Fix for OnUpdate
-- 2.02
-- + table (de)compression system
-- + ^^ Cache
-- 2.0001
-- + password protection on text
-- + modified base64 coding functions


local DUCKNET_VERSIONTEXT = "DuckNet v1.09";
local DUCKNET_VERSION_DATA = "003";	-- The data structure in use
local DUCKNET_VERSION_API = 2;
local DUCKNET_FROMCODE = "|";
local DUCKNET_TOCODE = "\1";			-- This MUST be only one character
local DUCKNET_DATA="\2";				-- NEW, old="!"
local DUCKNET_ENTRY="\3";				-- NEW, old="#"
local DUCKNET_COMMAND="\4";				-- NEW, old=":"
local DUCKNET_FREESPLIT1="\5";			-- Free splitter for addons
local DUCKNET_QUOTATION="\6";			-- NEW, old="\""
local DUCKNET_WNSPLIT1="\7";			-- WoWnet splitter 1
local DUCKNET_DISCONNECT="\10";			-- Never use this
local DUCKNET_LINEBREAKnative = "\n";
local DUCKNET_LINEBREAKhtml = "<BR/>";
local DUCKNET_ENTRYBREAKCONTINUE = "<EBC/>";
local DUCKNET_BOOLVALDATA="BoolVal:";

local DUCKNET_NEG_STOPPED = -1;
local DUCKNET_NEG_RUNNING = 0;
local DUCKNET_NEG_DONE = 5;

local DUCKNET_HEARTBEAT = 0.3;
local DUCKNET_TRANS_STOP = 0;
local DUCKNET_TRANS_START = 1;

DUCKNET_REQ_NEWDATA = "DuckNet-Req-New-Data";
DUCKNET_REQ_RETRANSMIT = "DuckNet-Req-Retransmit-Data";
DUCKNET_ACT_TRANSMIT = "DuckNet-Act-Transmit-Data";
DUCKNET_ACT_TRANSMITDONE = "DuckNet-Act-Transmit-Data-Done";
DUCKNET_ACT_MYSTAMP = "DuckNet-Act-MyStamp";


if (not DuckMod) then						-- Make it if empty
	DuckMod = {};
end


if (not DuckMod[TV]) then					-- Don't mess with it if it's loaded

-- Helper colours
DuckMod[TV]={								-- Add to array
	Color = {
		White="|cFFFFFFFF",
		Green="|cFF00FF00",
		Red="|cFFFF0000",
		lBlue="|cFF6060FF",				-- Light blue
		hBlue="|cFEA0A0FF",				-- Highlight blue
		Yellow="|cFFFFFF00",
	},
};

local DM=DuckMod[TV];


-- Make a random number of integer size with fractional part
function DM:Random(bottom,top)
	if (not bottom) then
		bottom=0;
		top=1;
	elseif (not top) then
		top=bottom;
		bottom=0;
	end

	local num=math.random();		-- Generate a fractional number
	num=num*(top-bottom);			-- Set span
	num=num+bottom;					-- Set floor
	return num;
end


-- Safe table-copy with optional merge (equal entries will be overwritten)
function DM:CopyTable(t,new)
	if (not t and not new) then return nil; end
	if (not t) then t={}; end
	if (not new) then new={}; end
	local i,v;
	for i,v in pairs(t) do
		if (type(v)=="table") then new[i]=self:CopyTable(v,new[i]); else new[i]=v; end
	end
	return new;
end

-- Safe table-clear
function DM:ClearTable(t)
	for k in pairs(t) do t[k]=nil; end;
end

-- Return the common DuckMod ID
function DM:GetID(link)
	if (type(link)~="string") then
		if (type(link)~="number") then return nil,nil; end
		_,link=GetItemInfo(link);
	end
	local itemID,i1,i2,i3,i4,i5,i6=link:match("|Hitem:(%p?%d+):(%p?%d+):(%p?%d+):(%p?%d+):(%p?%d+):(%p?%d+):(%p?%d+):(%p?%d+)|h%[(.-)%]|h");
	if (not i6) then
		itemID,i1,i2,i3,i4,i5,i6=link:match("item:(%p?%d+):(%p?%d+):(%p?%d+):(%p?%d+):(%p?%d+):(%p?%d+):(%p?%d+)");
	end
	if (not i6) then return; end
	return "item:"..itemID..":"..i1..":"..i2..":"..i3..":"..i4..":"..i5..":"..i6,tonumber(itemID);
end

-- Print text
function DM:Chat(msg,r,g,b)
	if (DEFAULT_CHAT_FRAME) then
		if (not r and not g and not b) then r=1; g=1; b=1; end;
		if (not r) then r=0; end;
		if (not g) then g=0; end;
		if (not b) then b=0; end;
		DEFAULT_CHAT_FRAME:AddMessage(msg,r,g,b);
	end
end


-- Multi-threading utility
-- DM.MT:Set(func)
--    Register a function for MT

DM.MT={
	LastStack="",
	Current=0,
	Count=0,
	Speed=(1/30),
	LastTime=0,
	Threads={
	},
};

function DM.MT:Run(name,func,...)
	for _,tTable in pairs(self.Threads) do
		if (tTable.orig==func) then return; end
	end
	self.RunningCo=true;
	self.Count=self.Count+1;
--DM:Chat("Starting "..name);
	self.Threads[self.Count]={};
	self.Threads[self.Count].name=name;
	self.Threads[self.Count].orig=func;
	self.Threads[self.Count].cr=coroutine.create(func);
	self.LastTime=GetTime();
	self.LastStack="Running "..name;
	local succeeded,result=coroutine.resume(self.Threads[self.Count].cr,...);
	if (not succeeded and Swatter) then
		if (Swatter) then Swatter.OnError(result,nil,self.LastStack);
		else DM:Chat(result); DM:Chat(self.LastStack); end
	end
	self.RunningCo=nil;
end

function DM.MT:Yield(immediate,dbdata)
	self.LastStack=debugstack(2);
	local now=GetTime();
	if (not immediate) then
		if (now-self.LastTime<self.Speed) then return; end
	end
	self.LastTime=now;		-- Inaccurate to account for other snags
	coroutine.yield();
	self.LastStack=debugstack(2);
end

function DM.MT:Next()
	if (self.RunningCo) then return; end	-- Don't if we are already doing it. In case of real MT.
	if (not self.Threads[self.Current+1]) then self.Current=0; end	-- Wrap
	self.Current=self.Current+1;
	if (not self.Threads[self.Current]) then return; end	-- Nothing to do
	if (coroutine.status(self.Threads[self.Current].cr)=="dead") then
		local removeIt=self.Current;
--DM:Chat("Ending "..self.Threads[removeIt].name);
		while (self.Threads[removeIt]) do
			self.Threads[removeIt]=self.Threads[removeIt+1];
			removeIt=removeIt+1;
		end
		self.Current=self.Current-1;
		self.Count=self.Count-1;
	else
		self.RunningCo=true;
		local succeeded,result=coroutine.resume(self.Threads[self.Current].cr);
		if (not succeeded) then
			if (Swatter) then Swatter.OnError(result,nil,self.LastStack);
			else DM:Chat(result); DM:Chat(self.LastStack); end
		end
 		self.RunningCo=nil;
	end
end

function DM.MT:Processes()
	return self.Count;
end


-- Table compression utility
-- API:
-- DuckMod.Table:Init(who,UseCache,Base)
--    Set default options for your addon.
--       who      - An identifier for your add-on.
--       UseCache - Use cached mode
--       Base     - Set a default table for read/write
--
-- DuckMod.Table:Read(who,entry,base)
--    Read a compressed entry
--       who   - Your addon's ID, if any
--       entry - The entry you want to read
--       base  - The base table where 'entry' is located. If omitted,
--               the default setting for your addon will be used.
--
-- DuckMod.Table:Write(who,entry,tData,base)
--     Write an uncompressed entry
--       who   - Your addon's ID, if any
--       entry - The entry you want to write
--       tData - Data write to 'entry'
--       base  - The base table where 'entry' is located. If omitted,
--               the default setting for your addon will be used.

DM.Table={
	Scrap={},
	Default={
		Default={
			UseCache=true,
			Base=nil,
		},
	},
	Entry ="\1",			-- 
		String="\1\1",
		Number="\1\2",
		Bool  ="\1\3",
		Nil   ="\1\4",
		Other ="\1\9",
	sTable="\2",			-- 
	eTable="\3",			-- 
	Version="\4",			-- 
		ThisVersion="1",
	Last="\4",
};



function DM.Table:Init(who,UseCache,Base)
	if (who=="Default") then return; end
	if (not Base) then return nil; end
	if (not self.Default[who]) then self.Default[who]={}; end
	wipe(self.Default[who]);
	self.Default[who].UseCache = UseCache;
	self.Default[who].Base     = Base;
	self.Default[who].Cache    = {};
end

function DM.Table:GetType(typedata,tdExtra)
	if (type(typedata)=="string") then return self.String; end
	if (type(typedata)=="number") then return self.Number; end
	if (type(typedata)=="boolean") then return self.Bool; end
	if (type(typedata)=="table") then return self.sTable..string.char(tdExtra); end
	if (not typedata) then return self.Nil; end
	return self.Other;
end

--tostring(123)  --Returns "123"
--tostring({})  --Returns "table: ###"
--tostring(function() end) --Returns "function: ###"
--tostring(nil) --Returns "nil"
--tostring(true) --Returns "true"

--[[
	MyTable = {
		First = {
			fdata=5,
		}
		second = {
			sdata="text",
		}
	}

	base[MyTable]="
	'Version''ThisVersion'sTable1'	-> Write
	'String'First'sTable2'			-> Compress (1)
	'String'fdata'Number'			-> Compress (2)
	5								-> Compress (3)
	'eTable2'						-> Compress (2)
	'String'second'sTable2'			-> Compress (1)
	'String'sdata'String'			-> Compress (2)
	text							-> Compress (3)
	'eTable2'						-> Compress (2)
	'eTable1'						-> Compress (1)
]]

function DM.Table:CompressV1(tData,level)
	local text="";
	if (type(tData)~="table") then return tostring(tData); end
	for entry,eData in pairs(tData) do
		text=text..self:GetType(entry)..entry..self:GetType(eData,level)..self:CompressV1(eData,level+1);
		if (type(eData)=="table") then
			text=text..self.eTable..string.char(level);
		end
	end
	return text;
end

function DM.Table:Write(who,entry,tData,base,section)
	local cache=self.Default[who].UseCache;
	if (not who) then who="Default"; end
	if (not base) then
		base=self.Default[who].Base;
	elseif (section and base~=self.Default[who].Base[section]) then		-- Only cache for base data
		cache=false
	end
	base[entry]=self.Version..self.ThisVersion..self:GetType(tData,100)..self:CompressV1(tData,101);
	base[entry]=base[entry]..self.eTable..string.char(100);

	if (cache) then
		if (base==self.Default[who].Base) then
			if (section) then
				if (not self.Default[who].Cache[section]) then self.Default[who].Cache[section]={}; end
				cache=self.Default[who].Cache[section];
			else
				cache=self.Default[who].Cache;
			end
			cache[entry]=tData;
		end
	end
	return true;
end

--[[
	index
	  1   2 -> Entry-type
	  3   a -> Entry name
	a+1 a+2 -> Data type
	a+3   b -> Data
]]

function DM.Table:PullEntry(cString)
	local stop1,stop2,stop3;
	if (cString:sub(1,1)==self.sTable) then
		stop1=cString:find(self.eTable..cString:sub(2,2),3,true);
		if (not stop1) then return nil; end	-- Missing data
		return cString:sub(1,2),cString:sub(3,stop1-1),cString:sub(stop1+2);
	end
	stop1=cString:find(self.Entry,3,true); if (not stop1) then stop1=cString:len()+1; end	-- Virtual entry position
	stop2=cString:find(self.sTable,3,true); if (not stop2) then stop2=cString:len()+1; end	-- Virtual entry position
	stop3=cString:find(self.eTable,3,true); if (not stop3) then stop3=cString:len()+1; end	-- Virtual entry position
	if (stop1>stop2) then stop1=stop2; end
	if (stop1>stop3) then stop1=stop3; end
	return cString:sub(1,2),cString:sub(3,stop1-1),cString:sub(stop1);
end

function DM.Table:DecompressV1(base,cString,anon)
--DM:Chat("Decompressing...");
	while(cString:len()>0) do
		local eType,eData=nil,nil;
		local dType,dData=nil,nil;
		if (anon) then
			dType,dData,cString=self:PullEntry(cString);
			dData=self:DecompressV1(base,dData);	-- dData is now without table-tags
		else
			eType,eData,cString=self:PullEntry(cString);		-- Entry name
			if (eType==self.Number) then eData=tonumber(eData); end
			dType,dData,cString=self:PullEntry(cString);		-- Entry data
			if (dType==self.Number) then dData=tonumber(dData);
			elseif (dType==self.Bool) then if (dData=="true") then dData=true; else dData=false; end
			elseif (dType==self.Nil) then dData=nil;
			elseif (dType:sub(1,1)==self.sTable) then
				base[eData]={};
				dData=self:DecompressV1(base[eData],dData);	-- dData is now without table-tags
			end
		end
		if (not base) then return dData;
		else
			if (anon) then base=dData;
			else base[eData]=dData; end
		end
	end
	return base;
end

-- "\1" 
-- "\2" 
-- "\3" 
-- "\4" 
--[[

["Smilin' Slirk Brassknob"] d
	Y 85.925
	X 41.105
	Zone The Storm Peaks - K3
	Faction Neutral
	Items e
		item:35950:0:0:0:0:0:0 f
			Name Sweet Potato Bread
			Count -1
		f
		item:35954:0:0:0:0:0:0 f
			Name Sweetened Goat's Milk
			Count -1
		f
		item:33445:0:0:0:0:0:0 f
			Name Honeymint Tea
			Count -1
		f
		item:33449:0:0:0:0:0:0 f
			Name Crusty Flatbread
			Count -1
		f
		item:33444:0:0:0:0:0:0 f
			Name Pungent Seal Whey
			Count -1
		f
	e
d",
]]

--	Entry ="\1", 
--	sTable="\2", 
--	eTable="\3", 


function DM.Table:Read(who,entry,base,section)
	local cache=self.Default[who].UseCache;
	if (not who) then who="Default"; end								-- Using default settings
	if (not base) then
		base=self.Default[who].Base;
	elseif (section and base~=self.Default[who].Base[section]) then		-- Only cache for base data
		cache=false
	end
	if (not base[entry]) then return nil; end							-- Entry does not exist
	if (type(base[entry])~="string") then			-- Not a proper entry
		DM:Chat("Got "..type(base[entry]).." as entry");
		return nil;
	end
--	if (base[entry]:byte(1)~=self.Version) then return base[entry]; end	-- It's uncompressed data
	local fnDecompress=self["DecompressV"..base[entry]:sub(2,2)];		-- Look for decompressor
	if (not fnDecompress) then return nil; end		-- We don't have a decompressor for this data
	-- Not using cache, so decompress to scrap-book and return it
	if (not cache) then
		wipe(self.Scrap);
		return DM:CopyTable(fnDecompress(self,self.Scrap,base[entry]:sub(3),true));		-- Strip version
	end
	if (section) then
		if (not self.Default[who].Cache[section]) then self.Default[who].Cache[section]={}; end
		cache=self.Default[who].Cache[section];
	else
		cache=self.Default[who].Cache;
	end
	-- Not decompressed yet, so decompress to cache and return it
	if (not cache[entry]) then
--		cache[entry]=DM:CopyTable(fnDecompress(self,self.Scrap,base[entry]:sub(3),true));
		cache[entry]={};
		fnDecompress(self,cache[entry],base[entry]:sub(3),true);
	end
	return cache[entry];
end

function DM.Table:PurgeCache(who)
	wipe(self.Default[who].Cache);
	collectgarbage("collect");
end


DM.Code={};

--[[
	Code with password. It's not complicated, but needs the password
	to decode, and is totally unreadable after coding.
		code=mod(byte+seed+pw[next],256);
		byte=mod(code-pw[next]-seed,256);
]]
function DM.Code:Protect(text,pw)
	if (not pw or pw=="") then return text; end
	local seed=0;
	local pwlen=pw:len();
	for i=1,pwlen do seed=seed+pw:byte(i); end
	local code="";
	local pi=1;
	for i=1,text:len() do
		code=code..string.char(mod(text:byte(i)+(seed+pw:byte(pi)),256));
		pi=pi+1; if (pi>pwlen) then pi=1; end
	end
	return self:Code(code);		-- Results in pure 8-bit, so we must code it
end

function DM.Code:Unprotect(code,pw)
	if (not pw or pw=="") then return code; end
	local decoded;
	code,decoded=self:Decode(code);
	if (decoded~=true) then return code; end
	local seed=0;
	local pwlen=pw:len();
	for i=1,pwlen do seed=seed+pw:byte(i); end
	local text="";
	local pi=1;
	for i=1,code:len() do
		local value=mod(code:byte(i)-(seed+pw:byte(pi)),256);
		while(value<0) do value=value+256; end
		text=text..string.char(value);
		pi=pi+1; if (pi>pwlen) then pi=1; end
	end
	return text;
end

--[[
	Text-code any three 8-bit values to four values ranging from 64-127
	inclusive.
		composite=byte1*65536+byte2*256+byte3
		char1=mod(composite,64)+64;
		composite=composite/64;
		char2=mod(composite,64)+64;
		composite=composite/64;
		char3=mod(composite,64)+64;
		char4=composite/64+64;

		composite=(char4-64)*262144+(char3-64)*4096+(char2-64)*64+(char1-64);
		char3=mod(composite,256);
		composite=composite/256;
		char2=mod(composite,256);
		char1=composite/256;
]]

--	raw string bytes:
--	11111111 22222222 33333333

--	Binary coding: +64 >> 7
--	111111110000000000000000
--	000000002222222200000000
--	000000000000000033333333

-->	111111112222222233333333 -> com[0]=0333333d
--	000000111111112222222233 -> com[1]=0222233c
--	000000000000111111112222 -> com[2]=0112222b
--	000000000000000000111111 -> com[3]=0111111a
function DM.Code:Code(text)
	text=text:len()..":"..text;
	while (math.floor(text:len()/3)*3~=text:len()) do text=text.." "; end
	local code="";
	for i=1,text:len(),3 do
		local com=bit.lshift(text:byte(i),16)+bit.lshift(text:byte(i+1),8)+text:byte(i+2);
		code=code..string.char(mod(com,64)+64); com=bit.rshift(com,6);
		code=code..string.char(mod(com,64)+64); com=bit.rshift(com,6);
		code=code..string.char(mod(com,64)+64); com=bit.rshift(com,6);
		code=code..string.char(com+64);
	end
	return code;
end

--	raw string bytes:
--	0333333d 0222233c 0112222b 0111111a

--	Binary coding:
--	111111000000000000000000
--	000000112222000000000000
--	000000000000222233000000
--	000000000000000000333333
-->	111111112222222233333333 -> inp=33333333
--	000000001111111122222222 -> inp=22222222 33333333
--	000000000000000011111111 -> inp=11111111 22222222 33333333

-- Return: text (string), decoded (true|false)
function DM.Code:Decode(code)
	local text="";
	if (mod(code:len(),4)~=0) then return code,false; end
	for i=1,code:len(),4 do
		local com;
		com=bit.lshift(code:byte(i+3)-64,18);
		com=com+bit.lshift(code:byte(i+2)-64,12);
		com=com+bit.lshift(code:byte(i+1)-64,6);
		com=com+(code:byte(i)-64);
		local inp=string.char(mod(com,256)); com=bit.rshift(com,8);
		inp=string.char(mod(com,256))..inp; com=bit.rshift(com,8);
		inp=string.char(mod(com,256))..inp;
--		inp=string.char(com)..inp;
		text=text..inp;
	end
	local here=text:find(":");
	if (not here) then return code,false; end
	local size=text:sub(1,here-1); size=tonumber(size);
	if (not size or size<1) then return code,false; end
	text=text:sub(here+1);
	size=text:len()-size;
	if (size>3 or size<0) then return "",true; end
	if (size>0) then text=text:sub(1,text:len()-size); end
	return text,true;
end

-- WoWnet
DM.WoWnet={
	prefix="Wnet",
	Connected=nil,				-- Not connected yet
	Session={					-- Data and functions specific for sessions
		LoopBack=nil,			-- Running in loopback mode. i.e playing the madman (talk to self)
		LastPubOut=nil,			-- Last data sent to public channel
		cbSearchResult=nil,		-- Callback: Incoming search-results
	},
	LinkColour="ffff8080";
};

-- The host-data for all servers on this ream/faction.
WoWnetServer={			-- Complete server contents (this server)
};

--[[
	host
		section
			SECTIONDATA
				Keys="key1,key2,...,keyN"
				Data=page-data
				Description="Short description"
			subsection
				...
----------------------------------------
	contents
		host1
			section 1
				section 1/1
					section 1/1/1
					section 1/1/2
				section 1/n
			section 2
				section 2/1
		host2
			...
]]

function DM.WoWnet:Link(location,text)
	return "|c"..DM.WoWnet.LinkColour.."|Hwownet:"..location.."|h["..text.."]|h|r";
end


-- contents -> Table for server contents
function DM.WoWnet:Connect(cbSR,cbID)
	local channel="WoWnet"
--	if (not JoinChannelByName(channel)) then
	if (not JoinTemporaryChannel(channel)) then
		local count=1
		self.Connected=nil;
		DM:Chat("Someone has blocked the WoWnet channel on this server.");
		while(not JoinTemporaryChannel("WoWnet"..count)) do
			count=count+1;
		end
		channel="WoWnet"..count;
		DM:Chat("Open WoWnet found at "..channel);
	end
	self.Connected=GetChannelName(channel);
	DM:Chat("Connected to WoWnet - "..type(self.Connected).." - "..self.Connected);
	if (self.Connected) then
		self.Session.cbSearchResult=cbSR;
		self.Session.cbInData=cbID;
		RegisterAddonMessagePrefix(DM.WoWnet.prefix);
--		DM.Table:Init(DM.WoWnet.prefix,nil,WoWnetServer);
	else
		self.Session.cbSearchResult=nil;
		self.Session.cbInData=nil;
	end
	return self.Connected;
end


--	WoWnetServer["Jäähinen"]={
--		home={
--			SECTIONDATA={
--				Keys="blog,Who da man?",
--				Data="<html><body><h1>SimpleHTML Demo: Ambush</h1><img src=\"Interface\\Icons\\Ability_Ambush\" width=\"32\" height=\"32\" align=\"right\"/><p align=\"center\">|cffee4400'You think this hurts? Just wait.'|r</p><br/><br/><p>Among every ability a rogue has at his disposal,<br/>Ambush is without a doubt the hardest hitting Rogue ability.</p></body></html>",
--				Description="Jäähinen's home-page.",
--			},
--		},
--	};

-- Traverse all keys for this section and collate matched locations
function DM.WoWnet:CheckKeys(section,sTable,key)
	local list={};
	for entry,eTable in pairs(sTable) do
		if (entry~="SECTIONDATA") then
			list=DM:CopyTable(self:CheckKeys(section.."/"..entry,eTable,key),list);
		else
			if (eTable.Keys) then
				local param={strsplit(",",eTable.Keys)};
				for _,tag in pairs(param) do
					tag=string.lower(strtrim(tag));
					if (tag==key) then
						if (not eTable.Description) then list[section]="<no description>";
						else list[section]=eTable.Description; end
						break;
					end
				end
			end
		end
	end
--[[
	local list={};
	for entry,eTable in pairs(sTable) do
		if (eTable.SECTIONDATA.Keys) then
			local param={strsplit(",",eTable.SECTIONDATA.Keys)};
			for _,tag in pairs(param) do
				tag=string.lower(strtrim(tag));
				if (tag==key) then
					if (not eTable.SECTIONDATA.Description) then list[section]="<no description>";
					else list[section]=eTable.SECTIONDATA.Description; end
					break;
				end
			end
		end
		for subsection,ssTable in pairs(eTable) do
			if (subsection~="SECTIONDATA") then
				list=DM:CopyTable(self:CheckKeys(section.."/"..subsection,ssTable,key),list);
			end
		end
	end
]]
	return list;
end

-- Traverse all hosts for the key
function DM.WoWnet:FindKey(key)
	key=string.lower(strtrim(key));
	local list={};
	for host,hTable in pairs(WoWnetServer) do
		list[host]={};
		for section,sTable in pairs(hTable) do
			list[host]=DM:CopyTable(self:CheckKeys("/"..section,sTable,key),list[host]);
		end
	end
	return list;
end

-- This function receives all information from the WoWnet channel
-- sender -> Data originator
-- text -> The data
function DM.WoWnet:Input(sender,text)
	-- A search-phrase received
	if (text:find("search:",1,true)==1) then
		text=string.sub(text,8);
		-- Get section-list for supplied keys
		local param={strsplit(",",text)};
		local list={};
		local index=1;
		while (param[index]) do						-- Do all supplied keys
			local found=self:FindKey(param[index]);
			list=DM:CopyTable(found,list);				-- Overlay findings
			index=index+1;
		end
		-- Check the list
		local found=nil;
		for host,hTable in pairs(list) do
			found=true;
			break;
		end
		if (not found) then return; end				-- No matches at all
--TestDebugTable=DM:CopyTable(list);
		-- There are matches, so "answer the general, Baldrick"
		for host,hTable in pairs(list) do
			for section,desc in pairs(hTable) do
				self.Session:SendContents(sender,host,section..DUCKNET_WNSPLIT1..desc);
			end
		end
	elseif (text:find("goloc:",1,true)==1) then
--		DM:Chat("Location request: "..text:sub(7),1);
		self.Session:HandleAddress(sender,text:sub(7));
	end
end

function DM.WoWnet.Session:HandleAddress(sender,text)
	local splitter=text:find("/",1,true);
	local who,loc;
	if (splitter) then
		who=text:sub(1,splitter-1);
		loc=text:sub(splitter+1);	-- Not +1 since we keep the "/" in front
	else
		who=text;
		loc=""
	end
--	DM:Chat("Testing who: "..who,1);
	if (not WoWnetServer[who]) then return; end
--	DM:Chat(who.." is here.",1);
	if (loc=="") then
		loc="home";
		if (not WoWnetServer[who]["home"]) then
			loc="default";
			if (not WoWnetServer[who]["default"]) then
				return;
			end
		end
	end
	if (not WoWnetServer[who][loc]["SECTIONDATA"]) then
		return;
	end
	self:SendData(sender,text,WoWnetServer[who][loc]["SECTIONDATA"])
end

function DM.WoWnet.Session:Search(data)
	self:SendPublic("search:"..data);
end

function DM.WoWnet.Session:GoTo(data)
	DM.Net:ConnectW(DM.WoWnet.prefix,DM.WoWnet.Session.cbInData,"");
	self:SendPublic("goloc:"..data);
end

function DM.WoWnet.Session:SendPublic(data)
	DM.WoWnet.Session.LastPubOut=data;
	SendChatMessage(data,"CHANNEL",nil,DM.WoWnet.Connected);
end

function DM.WoWnet.Session:HandleLink(link)
	local lType,lAddress=strsplit(":",link);
	if (lType=="wownet") then
--		DM:Chat("Clicked "..lType.." link: "..lAddress,1);
		DM.WoWnet.Session:GoTo(lAddress);
	end
end

-- receiver -> The real user to send the message to
-- host -> The host to request a session with ("user" may be an alt of the host)
-- data -> Data to send
function DM.WoWnet.Session:SendContents(receiver,host,data)
--	DM:Chat("-> WN user: "..receiver,1);
--	DM:Chat("-> WN host: "..host,1);
--	DM:Chat("-> WN data: "..data,1);
	if ((string.len(host)+3+string.len(data)+1)>255) then return nil; end
	DM.WoWnet.Session:SAM("wNc"..host..":"..data,"WHISPER",receiver);
	return true;
end

-- receiver -> Who to /w to
-- location -> The address of the data following
-- data - The data for this location
function DM.WoWnet.Session:SendData(receiver,location,data)
--DM:Chat("ConnectW to "..receiver,1);
	DM.Net:ConnectW(DM.WoWnet.prefix,DM.WoWnet.Session.cbInData,receiver);
--DM:Chat("Sending table for "..location,1);

--	DM.Net:SendTable(DM.WoWnet.prefix,data,location);	-- 101 is base nested table ASCII code

	local prefix=DM.WoWnet.prefix;
	if (not DM.Net:CanTransmit(prefix)) then return false; end					-- Can't transmit now
	DM.Net:ClearOutput(prefix);
	DM.Net:AddKey(prefix,"S",0);
	DM.Net:AddKey(prefix,"M","SendData");
	DM.Net:AddKey(prefix,"A",DUCKNET_ACT_TRANSMIT);
	DM.Net:NewLine(prefix);
	local text;
	text=DM.Code:Code(DM.Table:CompressV1(data,101));
	if (not DM.Net:AddEntry(prefix,location,text)) then return nil; end
	return DM.Net:DoTransmission(prefix);
end

-- SAM: Send Addon Message
function DM.WoWnet.Session:SAM(a1,a2,a3)
	SendAddonMessage(DM.WoWnet.prefix,a1,a2,a3);
--	if (a4==UnitName("player")) then
--		DM:Chat("-> LOOPBACK",1);
--		DM.Net:OnEvent("CHAT_MSG_ADDON",a1,a2,a3,a4);
--	end
end

function DM.WoWnet.Session:SendError(prefix,header,message)
end

function DM.WoWnet.Session:Error(header,message)
	DM:Chat(header..": "..message,1);
end

-- wNr -> Request
-- wNc -> Contents
-- wNd -> Data

-- Called when a "WHISPER" with "wNc" is received, denoting incoming contents-entry
-- sender -> Player to send info to
-- host -> Host for which contents is requested
-- data -> section DUCKNET_WNSPLIT1 description
function DM.WoWnet.Session:Contents(sender,host,data)
--DM:Chat("Ready...",1);
	if (not self.cbSearchResult) then return; end
--DM:Chat("Cleared...",1);
	self.cbSearchResult(sender,host,strsplit(DUCKNET_WNSPLIT1,data));
end

--[[
-- Called when a "WHISPER" with "wNr" is received, denoting a request for a page
function DM.WoWnet.Session:Request(sender,host,data)
	local prefix="nWd"..host;
	-- Set up a channel with requester
	DM.Net:ConnectW(prefix,
					DM.WoWnet.Session.RX,	-- RX callback
					DM.WoWnet.Session.INFO,	-- INFO callback
					sender);				-- Receiver of data
	-- Find host
	if (not WoWnetServer[host]) then
		self:SendError(prefix,"Fatal error","The host \""..host.."\" were not found.");
		return true;		-- Set input as "Handled"
	end

	-- Find section
	local loc={strsplit("/",data)};
	local index=1;
	local here=WoWnetServer[host];
	while(loc[index]) do
		if (not here[loc[index] ]) then
			self:SendError(prefix,"Fatal error","The destination \""..loc[index].."\" does not exist.");
			return true;		-- Set input as "Handled"
		end
		here=here[loc[index] ];
		index=index+1;
	end
	if (index==1) then		-- root requested
		if (not here.Default) then
			self:SendError(prefix,"Fatal error","No default destination at this host.");
			return true;		-- Set input as "Handled"
		end
		here=here.Default;
	end

--	section
--		SECTIONDATA
--			Keys="key1,key2,...,keyN"
--			Data=page-data
--			Description="Short description"

	local chunk={};
	chunk.Location=host.."/"..data;
	if (type(here.SECTIONDATA.Data)=="string") then chunk.Data=here.SECTIONDATA.Data;
	elseif (type(here.SECTIONDATA.Data)=="table") then chunk.Data=DM:CopyTable(here.SECTIONDATA.Data);
	else
		self:SendError(prefix,"Fatal error","Incomplete data at this destination.");
		return true;	-- Set input as "Handled"
	end

--	chunk
--		Location="host/section/subsection"
--		Data=string|table
	if (DM.Net:SendTable(prefix,chunk,"PageData")~=true) then
		self:Error("Fatal error","Could not send page-data.");
		return true;	-- Set input as "Handled"
	end

	return true;	-- Set input as "Handled"
end
]]

--[[
-- A table
function DM.WoWnet.Session.RX(input)
	if (not DM.WoWnet.Session.cbInData) then return; end
	DM.WoWnet.Session.cbInData(DM.WoWnet.Session.InMarker,input);
end
]]

-- A table
function DM.WoWnet.Session.INFO(input)
	local info=DM:CopyTable(input);

	local marker=nil;

	local seq=1;
	while (info[seq]) do
		if (info[seq].Type=="M") then marker=info[seq].Command; end
		seq=seq+1;
	end

	DM.WoWnet.Session.InMarker=marker;
	if (marker) then
		if (marker=="PageData") then
		end
	end
end


-- DuckNet
DM.Net={
	Split1=DUCKNET_FREESPLIT1,
	Timers = {
		Last=0,
		LastEntry=0,
	},
	DB = { },
	Rebuilding = nil,
	TheFrame=nil,

	DuckNet_Debug = false,
	DuckNet_DebugData = false,
};


function DM.Net:NegWindow()
	return DUCKNET_NEG_DONE;
end

function DM.Net:ConnectGeneric(channel,password)
	if (not JoinChannelByName(channel)) then self.Connected=nil; else self.Connected=true; end
	if (self.Connected) then
	else
	end
	return self.Connected;
end

-- Set up for handling
function DM.Net:Init()
	if (not self.TheFrame) then
		self.TheFrame=CreateFrame("Frame","DuckMod-Net-Messager"..TV,UIParent);
		if (not self.TheFrame) then
			Chat("Could not create a DuckMod frame",1);
			return;
		end
	end
	if (not self.TheFrameWorldMap) then
		self.TheFrameWorldMap=CreateFrame("Frame","DuckMod-Net-Messager"..TV,WorldMapDetailFrame);
		if (not self.TheFrameWorldMap) then
			Chat("Could not create a DuckMod frame (2)",1);
			return;
		end
	end
	self.TheFrame:RegisterEvent("CHAT_MSG_ADDON");				-- Guild Addon channel
	self.TheFrame:RegisterEvent("CHAT_MSG_CHANNEL");			-- WoWnet
	self.TheFrame:RegisterEvent("CHAT_MSG_CHANNEL_NOTICE");		-- WoWnet
	self.TheFrame:SetScript("OnEvent",DM.Net.OnEvent);
	self.TheFrame:SetScript("OnUpdate",DM.Net.HeartBeat);
	self.TheFrameWorldMap:SetScript("OnUpdate",DM.Net.HeartBeat);


	-- As LUA random can be dodgy at times, it is recommended to make
	-- a few calls to it to "get it going"
	math.random(); math.random(); math.random();
end


function DM.Net:Valid(prefix,ctype)
	if (not prefix) then return nil; end							-- None provided
	if (prefix==DM.WoWnet.prefix and DM.WoWnet.Connected and ctype=="WHISPER") then
		return true;
	end
	if (not self.DB[prefix]) then return nil; end				-- Provided, but not registered
	if (ctype) then
		if (self.DB[prefix].CType~=ctype) then return nil; end	-- Wrong channel type
	end
	return true;
end


-- The heartbeat. The OnUpdate handler
function DM.Net.HeartBeat(frame,elapsed)
--	local elapsed=arg1;
	if (not elapsed) then return; end;
	local self=DM.Net;

	DM.MT:Next();		-- Run multi-threading

	-- Second pass of rendering
	if (DM.RenderWnCounter==2) then DM:Render_2(DM.RU.Canvas,DM.RenderWnRemaining); end

	self.Timers.Last=self.Timers.Last+elapsed;					-- Update this
	if (self.Timers.Last<DUCKNET_HEARTBEAT) then return; end;	-- Heartbeat is DUCKNET_HEARTBEAT seconds
	self.Timers.Last=0;											-- Restart

	local now=time();
	for k,v in pairs(self.DB) do
		if (v.Alive) then										-- An alive-stamp exists (wownet)
			if ((now-v.Alive)>120) then self:Disconnect(k);		-- Delete this entry
			else self:HeartBeatCycle(k); end
		else
			self:HeartBeatCycle(k);								-- non-wownet
		end
	end

	-- Note that we're not using the accurate form. The reason is that this code
	-- does not need accurate timing, and this way has a much bigger chance of
	-- success if any hick-ups should occur elsewhere - thus increasing the chance
	-- of correct transmission.
end


-- Handler for a single prefix
function DM.Net:HeartBeatCycle(prefix)
	if (not self:Valid(prefix)) then
		if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: DuckNet_HeartBeatCycle called with prefix "..prefix); end;
		return 1;					-- Stop cycle
	end

	local now=time();

--[[		DuckNet negotiation		]]
	if (self.DB[prefix].Negotiate.Start) then
		self.DB[prefix].Negotiate.HoldOff=DUCKNET_NEG_RUNNING;
		self.DB[prefix].Negotiate.Start=false;
	end
	if (self.DB[prefix].Negotiate.HoldOff>=DUCKNET_NEG_RUNNING) then
		self.DB[prefix].Negotiate.HoldOff=self.DB[prefix].Negotiate.HoldOff+DUCKNET_HEARTBEAT;
		if (self.DB[prefix].Negotiate.HoldOff>=DUCKNET_NEG_DONE) then
			if (self.DB[prefix].CallBack.NegotiateWon) then		-- Check for callback-function
				local IWon=false;
				if (self.DB[prefix].Negotiate.SelfStamp>=self.DB[prefix].Negotiate.HighestStamp) then				-- I have highest stamp
					if (self.DB[prefix].Negotiate.HighestRoll<self.DB[prefix].Negotiate.Roll) then IWon=true; end;	-- I have highest roll
				end
				if (DuckNet_Debug) then
					if (IWon) then DM:Chat(prefix.." DEBUG: Negotiation won");
					else DM:Chat(prefix.." DEBUG: Negotiation lost"); end
				end
				self.DB[prefix].CallBack.NegotiateWon(IWon,self.DB[prefix].Negotiate.StartMarker);		-- Tell the calling addon if it won or not
				if (DuckMod_Present) then DuckMod_DN_NegotiationComplete(IWon); end
			elseif (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: Negotiation done");
			end
			self:StopNegotiation(prefix);
		end
	end

	if (self.DB[prefix].Transmitting>DUCKNET_TRANS_STOP) then	-- It's transmitting
		if (self.DB[prefix].TransmitDelay>0) then
			self.DB[prefix].TransmitDelay=self.DB[prefix].TransmitDelay-DUCKNET_HEARTBEAT;
		elseif (not self.DB[prefix].LastOutput) then					-- It's there now
			self:SendNextLine(prefix);							-- Duh
		end
	end

	-- Check broken incoming data
	if (self.DB[prefix].LastEntry>-1 and now-self.Timers.LastEntry>10) then
		self.DB[prefix].LastEntry=-1;								-- Stop receiving
		self.Rebuilding=nil;
	end

	if (self.DB[prefix].Common.Data and self.DB[prefix].Common.Boot>=0) then
		self.DB[prefix].Common.Boot=self.DB[prefix].Common.Boot-DUCKNET_HEARTBEAT;
		if (self.DB[prefix].Common.Boot<0) then
			-- Process boot
		end
	end

	return 0;
end


-- An event has been received
function DM.Net:OnEvent(event,arg1,arg2,arg3,arg4,_,_,_,_,arg9)
	if (event=="CHAT_MSG_ADDON") then
		-- Check for WoWnet input
		if (arg1==DM.WoWnet.prefix and arg3=="WHISPER") then
			if (arg2:find("wN")==1) then
--DM:Chat("<- "..arg2,1);
--DM:Chat("<- "..arg3,1);
--DM:Chat("<- "..arg4,1);
				local splitter=arg2:find(":");
				local host=arg2:sub(1,splitter-1);
				arg2=arg2:sub(splitter+1);
--DM:Chat("<- "..splitter,1);
--DM:Chat("<- "..host,1);
--DM:Chat("<- "..arg2,1);

				host=host:sub(3);
				if (host:find("r")==1) then			-- Request received
					host=host:sub(2);
					if (DM.WoWnet.Session:Request(arg4,host,arg2)) then return; end
				elseif (host:find("c")==1) then		-- Contents received
					host=host:sub(2);
-- DM:Chat("<- "..arg1,1); DM:Chat("<- "..arg2,1); DM:Chat("<- "..arg3,1); DM:Chat("<- "..arg4,1);
					if (DM.WoWnet.Session:Contents(arg4,host,arg2)) then return; end
				end
			end
		end
--arg1	prefix
--arg2	message
--arg3	distribution type ("PARTY", "RAID", "GUILD", "BATTLEGROUND" or "WHISPER")
--arg4	sender

		-- Normal input
		local instance=nil;
		if (DuckMod_Present) then instance=DuckMod_DN_CHAT_MSG_ADDON(arg1,arg2,arg3,arg4); else instance=arg4; end
		if (not DM.Net:Valid(arg1,arg3)) then return; end;					-- Quick way out without further function-calls - which ParseInput will do
		DM.Net:ParseInput(arg1,arg2,instance);								-- It's from a registered channel, so attempt to decode it
		return;
	end
	if (event=="CHAT_MSG_CHANNEL" and strlower(arg9:sub(1,6))=="wownet") then
		if (arg1~=DM.WoWnet.Session.LastPubOut) then
			DM.WoWnet:Input(arg2,arg1);
		else
			DM.WoWnet.Session.LastPubOut=nil;
		end
		return;
--arg1	chat message
--arg2	author
--arg8	channel number
--arg9	channel name without number (this is _sometimes_ in lowercase)
	end
	if (event=="CHAT_MSG_CHANNEL_NOTICE" and strlower(arg9)=="wownet") then
		return;
--arg1	"YOU_JOINED", "YOU_LEFT", or "THROTTLED"
--arg9	channel name without number (this is _sometimes_ in lowercase)
	end
end


-- Handles basic set-up for network negotiation
function DM.Net:StartNegotiation(prefix,stampmarker,selfstamp,linestamp)
	self.DB[prefix].Negotiate.StartMarker=stampmarker;
	self.DB[prefix].Negotiate.SelfStamp=selfstamp;
	if (not linestamp) then linestamp=selfstamp; end;
	self.DB[prefix].Negotiate.Start=true;											-- Start a negotiation
	self.DB[prefix].Negotiate.Roll=0;												-- Do not participate in roll yet
	self.DB[prefix].Negotiate.HighestStamp=0;
	self.DB[prefix].Negotiate.HighestRoll=0;

	if (selfstamp<=linestamp) then														--> I have same or lower
		self.DB[prefix].Negotiate.HighestStamp=linestamp;							-- I am not higher, so set incoming as highest
		if (DuckNet_Debug) then Chat(prefix.." DEBUG: NegStart -> I have same or lower stamp (my poll or incoming)"); end
		return nil;			-- Did not send data
	end

	self.DB[prefix].Negotiate.HighestStamp=selfstamp;							-- I am higher, so set it as highest
	self.DB[prefix].Negotiate.Roll=math.random(1000000);							-- Make my own random for transmission
	local output="";																-- Start line for transmission
	if (stampmarker) then output=output..self:MakeEntry("M",stampmarker); end	-- Add stampmarker if it has been received
	output=output..self:MakeEntry("S",self.DB[prefix].Negotiate.SelfStamp);	-- Add own stamp
	output=output..self:MakeEntry("R",self.DB[prefix].Negotiate.Roll);		-- Add own random number
	output=output..self:MakeEntry("A",DUCKNET_ACT_MYSTAMP);						-- Set action
	self:Out(prefix,output);														-- Send your random
	if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: NegStart -> Roll: "..self.DB[prefix].Negotiate.Roll); end
	return true;				-- Data sent
end


-- Kill it if need be
function DM.Net:StopNegotiation(prefix)
	self.DB[prefix].Negotiate.HighestStamp=0;
	self.DB[prefix].Negotiate.HighestRoll=0;
	self.DB[prefix].Negotiate.Start=false;								-- Don't start a negotiation
	self.DB[prefix].Negotiate.HoldOff=DUCKNET_NEG_STOPPED;				-- No negotiation is running
end


--[[ DuckNet Protocol 1 ]]
function DM.Net:ParseInput(prefix,text,instance)
	local function Closest()
		local small=string.find(text,DUCKNET_DATA); if (not small) then small=10000; end
		local Colon=string.find(text,DUCKNET_COMMAND); if (not Colon) then Colon=10000; end
		local Hash=string.find(text,DUCKNET_ENTRY); if (not Hash) then Hash=10000; end
		if (Colon<small) then small=Colon; end
		if (Hash<small) then small=Hash; end
		return small;
	end
	local function Next()
		local code=nil;
		local info=nil;
		local data=nil;
--[[		Commands			]]
		if (string.sub(text,1,1)==DUCKNET_COMMAND) then
			code=string.sub(text,1,2); text=string.sub(text,3);		-- Extract command
			local near=Closest();
			if (near>1) then info=string.sub(text,1,near-1); text=string.sub(text,near);		-- Extract the info
			else info=text; text=""; end														-- Use the rest of the line as info
--[[		Table entry			]]
		elseif (string.sub(text,1,1)==DUCKNET_ENTRY) then
			code=string.sub(text,1,2); text=string.sub(text,3);									-- Extract tag and type-indicator
			local near=Closest(); if (near>1) then												-- Locate next delimiter
				info=string.sub(text,1,near-1); text=string.sub(text,near);						-- Extract entry name
				if (string.sub(text,1,1)==DUCKNET_DATA) then
					text=string.sub(text,2);													-- Remove data-marker if data is present
					if (string.sub(text,1,1)=="\"") then
						text=string.sub(text,2); near=string.find(text,"\"");					-- Remove first quote. Find next quote
						if (not near) then text="\""..text; data=text; text="";					-- Not there. Reinsert previous. Copy. Save.
						else data=string.sub(text,1,near-1); text=string.sub(text,near+1);		-- Get it and remove including next quote
						end
					else
						near=Closest();				-- Not string, get next delimiter
						if (near==1) then data=nil;
						elseif (near>1) then data=tonumber(string.sub(text,1,near-1)); text=string.sub(text,near);
						else data=tonumber(text); text=""; end
					end
				end
			else info=text; text=""; end		-- Use all
		else info=text; text=""; end			-- Use all
		return code,info,data;
	end


--DM:Chat("...here 0...");
--[[	Validate input	]]
	if (not self:Valid(prefix)) then return; end		-- Validate incoming prefix
--DM:Chat("...here 0a...");

	-- InStamp also at own stuff
	local now=time();
	if (self.DB[prefix].CallBack.InStamp) then self.DB[prefix].CallBack.InStamp(now); end

	-- Base evaluation
--	if (self.DB[prefix].Receiver) then
--		self.DB[prefix].LastOutput=nil;			-- You're on /w. No echo ever comes.
--	end
--DM:Chat("...here 1...");
	if (text==self.DB[prefix].LastOutput) then self.DB[prefix].LastOutput=nil; return; end	-- Own transmission received
--DM:Chat("...here 2...");
	if (not text) then return; end
--DM:Chat("...here 3...");
	text=self:SwapText(text,DUCKNET_TOCODE,DUCKNET_FROMCODE);

	-- Check for broken lines, for concatenation
	local broken=nil;
	if (string.find(text,DUCKNET_ENTRYBREAKCONTINUE)==string.len(text)-(string.len(DUCKNET_ENTRYBREAKCONTINUE)-1)) then		-- Break-mark
		broken=string.sub(text,1,string.len(text)-string.len(DUCKNET_ENTRYBREAKCONTINUE));
	end
	if (broken and not self.Rebuilding) then self.Rebuilding=""; end	-- Start rebuilding
	if (self.Rebuilding) then											-- We are concatenating something
		if (broken) then												-- There's more coming
			self.Timers.LastEntry=now;
			self.Rebuilding=self.Rebuilding..broken;
			return;
		end
		text=self.Rebuilding..text;										-- We're done now
		self.Rebuilding=nil;											-- Go back to normal operation
	end

	-- Set-up for decoding
	local linestamp=0;
	local tp=self.DB[prefix].Data;					-- Pointer to the table root
	self.DB[prefix].Info={ };						-- Clear the info-table
	local infoline=1;									-- To build the info table
	local stampmarker=nil;								-- Stamp marker for this line
	local lastroll=0;

--[[	Cycle through the entire line of input	]]
	while(string.len(text)) do
		local tag,entry,data=Next();					-- Extract next entry
		if (not tag) then break; end;					-- Some tag-less info
		local entryn=tonumber(entry);					-- Better execution

		--[[                    ]]
		--[[   Common entries   ]]
		--[[                    ]]

		--[[ Stamp ]]
		if (tag==DUCKNET_COMMAND.."S") then
			linestamp=entryn;

		--[[ Random-data ]]
		elseif (tag==DUCKNET_COMMAND.."R") then
			lastroll=entryn;
			if (self.DB[prefix].Negotiate.HighestRoll<entryn) then
				self.DB[prefix].Negotiate.HighestRoll=entryn;
				if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: High roll received: "..entryn.." (My roll: "..self.DB[prefix].Negotiate.Roll..")");
				elseif (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: Low roll received: "..entryn.." (My roll: "..self.DB[prefix].Negotiate.Roll..")"); end
			end

		--[[ Stamp tag/marker ]]
		elseif (tag==DUCKNET_COMMAND.."M") then
			stampmarker=entry;
			self.DB[prefix].Negotiate.SelfStamp=self.DB[prefix].CallBack.CheckStamp(entry);		-- Get my addon's stamp
			if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: Marker received: "..entry); end
		end

		--[[ Incoming entry sequence (numeric) ]]
		if (tag==DUCKNET_COMMAND.."E") then
			entry=entryn;
			self.Timers.LastEntry=now;
			if (entry~=self.DB[prefix].LastEntry+1) then												-- Sequence is broken
				self:Out(prefix,DUCKNET_COMMAND.."A"..DUCKNET_REQ_RETRANSMIT);		-- Request the data retransmitted
			else
				self.DB[prefix].LastEntry=self.DB[prefix].LastEntry+1;							-- Increase sequence
			end

		--[[ DuckNet handling ]]
		elseif (tag==DUCKNET_COMMAND.."A" and entry==DUCKNET_REQ_RETRANSMIT) then					-- An addon received inconsistency and requires a re-transmit
			if (self.DB[prefix].Transmitting>DUCKNET_TRANS_STOP) then			-- I am currently transmitting
				self.DB[prefix].Transmitting=DUCKNET_TRANS_START;				-- Start from the top
				self.DB[prefix].TransmitDelay=2;									-- 2 seconds delay for retransmission
				if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: Restarting transmission"); end
			end

		--[[ Someone is gonna send stuff ]]
		elseif (tag==DUCKNET_COMMAND.."A" and entry==DUCKNET_ACT_TRANSMIT) then
			self:StopNegotiation(prefix);
			self.Timers.LastEntry=now;
			self.DB[prefix].LastEntry=0;													-- Nothing received yet
            self.DB[prefix].Data={ };													-- Clear input buffer
			if (self.DB[prefix].Transmitting>=DUCKNET_TRANS_START) then					-- I am transmitting too
				self.DB[prefix].Transmitting=DUCKNET_TRANS_STOP;							-- Stop transmission as several is trying to send
				self.DB[prefix].TransmitDelay=0;											-- No delay
				if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: Some other are sending while I am sending (or I am about to send). Aborting my transmission."); end;
				self.DB[prefix].CallBack.TX(self.DB[prefix].Transmitting,instance);	-- Notify addon about abort
			else
				if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: Incoming data..."); end
				if (DuckMod_Present) then DuckMod_DN_ACT_TRANSMIT(instance,stampmarker,linestamp); end
			end

		--[[ Inbound transmission is finished ]]
		elseif (tag==DUCKNET_COMMAND.."A" and entry==DUCKNET_ACT_TRANSMITDONE) then
			if (self.DB[prefix].Receiver) then		-- Decode and unpack data for WoWnet
				for nDataEntry,_ in pairs(self.DB[prefix].Data) do
					-- Repack all in a table, as outer table has not been coded
					self.DB[prefix].Data[nDataEntry]=DM.Table:GetType({},100)..DM.Code:Decode(self.DB[prefix].Data[nDataEntry])..DM.Table.eTable..string.char(100);	-- Decode
					self.DB[prefix].Data[nDataEntry]=DM.Table:DecompressV1({},self.DB[prefix].Data[nDataEntry],true);	-- Unpack
				end
			end
			if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: Incoming data done"); end
			self.DB[prefix].CallBack.RX(self.DB[prefix].Data);					-- Give table-pointer to registered addon
			self.DB[prefix].LastEntry=-1;										-- Nothing received yet
			if (DuckMod_Present) then DuckMod_DN_ACT_TRANSMITDONE(instance); end

		--[[ An addon has newer data than someone else ]]
		elseif (tag==DUCKNET_COMMAND.."A" and entry==DUCKNET_ACT_MYSTAMP) then						-- Some addon informs about it's stamp
			if (DuckMod_Present) then DuckMod_DN_Negotiation(instance,stampmarker,linestamp,lastroll); end
			if (linestamp>self.DB[prefix].Negotiate.HighestStamp) then			-- New stamp is highest of all
				self.DB[prefix].Negotiate.HighestStamp=linestamp;				-- Save it for test after negotiation timer
				self.DB[prefix].Negotiate.HighestRoll=lastroll;					-- Higher stamp, so set it's roll as highest
			end
			self.DB[prefix].Negotiate.HoldOff=DUCKNET_NEG_RUNNING;				-- Restart negotiation timer

			self.DB[prefix].Negotiate.SelfStamp=self.DB[prefix].CallBack.CheckStamp(stampmarker);	-- Someone sends their stamp, so get my addon's stamp
			if (self.DB[prefix].Negotiate.SelfStamp>self.DB[prefix].Negotiate.HighestStamp) then
				self.DB[prefix].Negotiate.Start=true;							-- Restart negotiating
				if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: ACT_MYSTAMP -> Lower stamp received. Restarting negotiation timer."); end
			elseif (self.DB[prefix].Negotiate.SelfStamp<self.DB[prefix].Negotiate.HighestStamp) then
				self:StopNegotiation(prefix);
				if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: ACT_MYSTAMP -> Higher stamp received. Stopping my negotiation."); end
			elseif (DuckNet_Debug) then
				DM:Chat(prefix.." DEBUG: ACT_MYSTAMP -> Equal stamp received with roll: "..lastroll.." - High roll is: "..self.DB[prefix].Negotiate.HighestRoll);
			end

		--[[ An addon wanna know whazzup ]]
		elseif (tag==DUCKNET_COMMAND.."A" and entry==DUCKNET_REQ_NEWDATA) then
			if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: Incoming request for transmit of data"); end
			if (DuckMod_Present) then DuckMod_DN_REQ_NEWDATA(instance,stampmarker,linestamp); end
			if (self.DB[prefix].Negotiate.Start or self.DB[prefix].Negotiate.HoldOff~=DUCKNET_NEG_STOPPED) then
				if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: REQ_NEWDATA -> Already negotiating. Stopping all current negotiation."); end
				self:StopNegotiation(prefix);
			else
				local thestamp=self.DB[prefix].CallBack.CheckStamp(stampmarker);
				local sentdata=self:StartNegotiation(prefix,stampmarker,thestamp,linestamp);
				if (DuckMod_Present and sentdata) then DuckMod_DN_Negotiation(instance,stampmarker,thestamp,self.DB[prefix].Negotiate.Roll); DuckMod_DN_Neg_SetMyStamp(thestamp); end
			end

			--[[ Commands to send to calling addon ]]
		elseif (string.sub(tag,1,1)==DUCKNET_COMMAND) then			-- Any command other than entry-number
			local tagtype=string.sub(tag,2,2);						-- Get the letter
			self.DB[prefix].Info[infoline]={};						-- Make (and clear) table
			self.DB[prefix].Info[infoline].Type=tagtype;			-- Save the command type
			if (tag==DUCKNET_COMMAND.."S") then
				self.DB[prefix].Info[infoline].Command=entryn;		-- Save the numeric command contents
			else
				self.DB[prefix].Info[infoline].Command=entry;		-- Save the command contents
			end
			infoline=infoline+1;
			if (DuckMod_Present) then DuckMod_Tag(prefix,instance,tag,entry); end
		end


--[[	Incoming table data 	]]
		if (string.sub(tag,1,1)==DUCKNET_ENTRY) then			-- >> Entry
			if (string.sub(tag,2,2)=="n") then					-- The entry-name is numeric
				entry=entryn;									-- Convert it to a number
			end
			if (data) then
				if (not self:Numeric(type(data))) then
					if (string.find(data,DUCKNET_BOOLVALDATA.."true")==1) then data=true;		-- Set boolean 'true'
					elseif (string.find(data,DUCKNET_BOOLVALDATA.."false")==1) then data=false;	-- Set boolean 'false'
					else 																		-- Recreate string
						data=self:SwapText(data,DUCKNET_LINEBREAKhtml,DUCKNET_LINEBREAKnative);
						data=self:SwapText(data,DUCKNET_QUOTATION,"\"");
					end
				end
				tp[entry]=data;	    							-- There's data, so store it in the entry as a label
			else												-- There's no data, so we're building (or adding to) a table below
				if (not tp[entry]) then tp[entry]={}; end		-- Generate empty table
				tp=tp[entry];									-- Set new table pointer
			end;
		end
	end   -- Text-loop

	-- The line contained info, so tell the addon
	if (infoline>1 and self.DB[prefix].CallBack.Info) then
		self.DB[prefix].CallBack.Info(self.DB[prefix].Info);
	end
end


function DM.Net:MakeBool(value)
	if (type(value)=="boolean") then return value; end
	if (not value) then return nil; end
	if (type(value)=="string") then value=tonumber(value); end
	if (type(value)=="number") then
		if (value==0) then return nil; end
		return true;
	end
	return nil;
end


-- Entry-builder
function DM.Net:MakeEntry(code,entry,data)
	if (not code) then return entry; end				-- Just return it
	if (not entry) then entry=tonumber(0); end			-- Just make sure it's a number

	local text="";
	local numeric=self:Numeric(type(entry));

	if (code==DUCKNET_ENTRY) then						-- It's an entry
		text=text..DUCKNET_ENTRY;						-- Set entry marker
		if (numeric) then text=text.."n";				-- It's a numeric entry
		else text=text.."s"; end						-- It's a string entry
	else text=text..DUCKNET_COMMAND..code; end			-- It's (probably) a command

	text=text..entry;									-- Add entry for supplied code

	if (code==DUCKNET_ENTRY and data) then				-- The entry carries data
		numeric=self:Numeric(type(data));				-- Check numeric
		local bool=self:Bool(type(data));				-- Check boolean
		text=text..DUCKNET_DATA;						-- Denote data
		if (not numeric) then							-- A string, so enclose
			text=text.."\"";
			if (bool) then
				if (data==true) then data=DUCKNET_BOOLVALDATA.."true"; else data=DUCKNET_BOOLVALDATA.."false"; end
			else
				data=self:SwapText(data,DUCKNET_LINEBREAKnative,DUCKNET_LINEBREAKhtml);	-- Transmittable linebreaks
				data=self:SwapText(data,"\"",DUCKNET_QUOTATION);
			end
		end
		text=text..data;																-- Add the entry data
		if (not numeric) then text=text.."\""; end										-- A string, so enclose
	end

	return text;
end


-- Handle further transmission
function DM.Net:SendNextLine(prefix)
	if (not self.DB[prefix].Output.Buffer[self.DB[prefix].Transmitting]) then		-- No more lines
		if (self.DB[prefix].PB) then self.DB[prefix].PB:SetValue(0); end
		local lines=self.DB[prefix].Transmitting;
		self.DB[prefix].Transmitting=DUCKNET_TRANS_STOP;								-- Not transmitting
		self.DB[prefix].CallBack.TX(lines);											-- Notify addon
		return;
	end

	if (self.DB[prefix].PB) then self.DB[prefix].PB:SetValue(self.DB[prefix].Transmitting); end
	self:Out(prefix,self.DB[prefix].Output.Buffer[self.DB[prefix].Transmitting]);		-- Send it
	self.DB[prefix].Transmitting=self.DB[prefix].Transmitting+1;												-- Go to next
end


-- Generic addon output
function DM.Net:Out(prefix,text)
	text=self:SwapText(text,DUCKNET_FROMCODE,DUCKNET_TOCODE);

	if (string.len(text)>254) then
		DM:Chat("Current line is too long ("..string.len(text)..")",1,0,0);
		return;
	end

-- Comment out these two to not get kicked at trans-debug
	SendAddonMessage(prefix,text,self.DB[prefix].CType,self.DB[prefix].Receiver);
	if (not self.DB[prefix].Receiver) then
		self.DB[prefix].LastOutput=text;
	end

	if (self.DuckNet_DebugData) then DM:Chat("! "..prefix.." "..text); end
end


--[[                   ]]
--[[ Support-functions ]]
--[[                   ]]


-- This function may seem a little overkill for a trained LUA programmer, but
-- better safe than sorry in case of any spec-changes
function DM.Net:SwapText(text,from,to)
	if (text) then
		while (string.find(text,from)) do text=string.gsub(text,from,to); end
	end
	return text;
end

-- Yeye... I know LUA natively does this (mostly), but this one restricts it a bit
function DM.Net:Numeric(typestring)
	if (typestring=="number" or typestring=="nil") then return true; end
	return nil;
end
function DM.Net:Bool(typestring)
	if (typestring=="boolean") then return true; end
	return nil;
end


--[[                                                                  ]]
--[[ Please note: The following 2 functions are still at a beta stage ]]
--[[                                                                  ]]

function DM.Net:AutoTableLinebreak(prefix,startnext)
	self:NewLine(prefix);													-- Advance

	if (table.maxn(self.DB[prefix].Output.Tables)<1) then
		if (startnext) then
			self.DB[prefix].Output.Buffer[self.DB[prefix].Output.Line]=DUCKNET_COMMAND.."E"..self.DB[prefix].Output.Entry;		-- !!! tp !!!
			self.DB[prefix].Output.Entry=self.DB[prefix].Output.Entry+1;
		end
	else
		for i,k in pairs(self.DB[prefix].Output.Tables) do							-- Iterate previous subtables
			if (not self:AddEntry(prefix,k,nil,true)) then return nil; end	-- Re-add them at next line
		end
	end
end


function DM.Net:CodeTable(prefix,t,level)
	local i,v;
	for i,v in pairs(t) do
		if (level==0) then self:NewLine(prefix); end							-- We're at the root and starting new entry, so make a new line
		if (type(v)=="table") then
			if (not self:AddTable(prefix,i)) then return nil; end				-- Add name of sub-table
			if (not self:CodeTable(prefix,v,level+1)) then return nil; end	-- Code sub-table
		else
			if (not self:AddEntry(prefix,i,v)) then return nil; end			-- Standard entry, so just add it with data
		end
	end

	table.remove(self.DB[prefix].Output.Tables);			-- Remove last table (default is last entry)
	self:AutoTableLinebreak(prefix);

	return true;
end


--[[  Checks if DuckNet is busy ]]
function DM.Net:NotCrusial(prefix)
	if (not self:Valid(prefix)) then return nil; end;	                            	-- No such prefix
	if (self.DB[prefix].LastEntry>-1) then return nil; end;							-- I am currently receiving
	if (self.DB[prefix].Transmitting>=DUCKNET_TRANS_START) then return nil; end;		-- I am currently transmitting
	if (self.DB[prefix].Negotiate.Start) then return nil; end;							-- I am currently starting a negotiation
	return true;
end



--[[                    ]]
--[[      User API      ]]
--[[                    ]]


function DM.Net:ReturnNull()
	return 0;
end

function DM.Net:ConnectW(prefix,cbRX,receiver)
	local ctype="WHISPER";
	self:Connect(prefix,ctype,nil,cbRX,nil,DM.WoWnet.Session.INFO,nil,nil,nil,nil);
	self.DB[prefix].Receiver=receiver;
	self.DB[prefix].Alive=time();
end

function DM.Net:Connect(prefix,ctype,ctDATA,cbRX,cbTX,cbINFO,cbCS,cbNW,cbIS,pb)
	if (DuckMod_Present) then DuckMod_OnLoad(); end

	self:Disconnect(prefix);
	if (not (ctype=="PARTY" or ctype=="RAID" or ctype=="GUILD" or ctype=="BATTLEGROUND" or ctype=="WHISPER")) then
		return nil;
	end;

	-- Define the addon
	RegisterAddonMessagePrefix(prefix);
	self.DB[prefix]={ };
	self.DB[prefix].CType=ctype;
	self.DB[prefix].PB=pb;
	self.DB[prefix].LastOutput=nil;
	self.DB[prefix].LastEntry=-1;
	self.DB[prefix].Negotiate={ };
	self.DB[prefix].Negotiate.Start=false;
	self.DB[prefix].Negotiate.HoldOff=DUCKNET_NEG_STOPPED;
	self.DB[prefix].Negotiate.SelfStamp=0;
	self.DB[prefix].Negotiate.HighestStamp=0;
	self.DB[prefix].Negotiate.HighestRoll=0;
	self.DB[prefix].Negotiate.Roll=-1;
	self.DB[prefix].CallBack={ };
	self.DB[prefix].CallBack.RX=cbRX;
	self.DB[prefix].CallBack.TX=cbTX;
	self.DB[prefix].CallBack.Info=cbINFO;
	self.DB[prefix].CallBack.CheckStamp=cbCS;
	self.DB[prefix].CallBack.NegotiateWon=cbNW;
	self.DB[prefix].CallBack.InStamp=cbIS;

	-- Set up common data-space
	self.DB[prefix].Common={};
	self.DB[prefix].Common.Data=ctDATA;
	self.DB[prefix].Common.Boot=30;

	-- Prep some tables
	self.DB[prefix].Info={ };
	self.DB[prefix].Data={ };
	self.DB[prefix].Output = { };
	self.DB[prefix].Output.Buffer = { };
	self.DB[prefix].Output.Tables = { };

	-- Fake some callbacks if not provided
	if (not self.DB[prefix].CallBack.CheckStamp) then self.DB[prefix].CallBack.CheckStamp=self.ReturnNull; end
	if (not self.DB[prefix].CallBack.TX) then self.DB[prefix].CallBack.TX=self.ReturnNull; end
	if (not self.DB[prefix].CallBack.RX) then self.DB[prefix].CallBack.RX=self.ReturnNull; end

	-- Make usable
	self.DB[prefix].TransmitDelay=0;						-- No delay
	self.DB[prefix].Transmitting=DUCKNET_TRANS_STOP;		-- Not transmitting
	self.DB[prefix].Idle=true;							-- Ready to work now

	self:StopNegotiation(prefix);				-- NEW
	self:ClearOutput(prefix);					-- NEW

	return true;
end

--function DuckNet_UnRegister(prefix)
function DM.Net:Disconnect(prefix)
	self.DB[prefix]=nil;
end


function DM.Net:Status(prefix)
	local text="";

	-- Valid
	if (self:Valid(prefix,"GUILD")) then text=text..DM.Color.Green;
	else text=text..DM.Color.Red; end
	text=text.."Net ";
	if (not self:Valid(prefix,"GUILD")) then return text; end
	-- Receiving
	if (self.DB[prefix].LastEntry>-1) then text=text..DM.Color.Red;
	else text=text..DM.Color.Green; end
	text=text.."Rx ";
	-- Transmitting
	if (self.DB[prefix].Transmitting>=1) then text=text..DM.Color.Red;
	else text=text..DM.Color.Green; end
	text=text.."Tx ";
	-- Negotiate start
	if (self.DB[prefix].Negotiate.Start) then text=text..DM.Color.Red;
	else text=text..DM.Color.Green; end
	text=text.."NegS ";
	-- Negotiate
	if (self.DB[prefix].Negotiate.HoldOff<DUCKNET_NEG_DONE and self.DB[prefix].Negotiate.HoldOff>DUCKNET_NEG_STOPPED) then text=text..DM.Color.Red;
	else text=text..DM.Color.Green; end
	text=text.."Neg ";
	-- Rebuilding
	if (self.Rebuilding) then text=text..DM.Color.Yellow;
	else text=text..DM.Color.Green; end
	text=text.."ReB ";

	-- Buffer
	text=text.."|rBuf:";
	if (self.DB[prefix].Output.Line==DUCKNET_TRANS_START) then text=text..DM.Color.Green;	-- Empty
	else text=text..DM.Color.Yellow; end
	text=text..self.DB[prefix].Output.Line;

	return text;
end


-- Return true: Data will be sent shortly
-- Return false: Data can't be sent now
-- Return nil: Data is unsendable
----DM.Net:SendTable(DM.WoWnet.prefix,data,location);
function DM.Net:SendTable(prefix,ADD_Data,ADD_BlockName,ADD_Stamp)
	if (not ADD_Data) then return nil; end
	if (not ADD_BlockName) then ADD_BlockName=prefix; end
	if (not ADD_Stamp) then ADD_Stamp=0; end
	if (type(ADD_Stamp)~="number") then return nil; end
	if (not self:CanTransmit(prefix)) then return false; end					-- Can't transmit now
	self:ClearOutput(prefix);
	self:AddKey(prefix,"S",ADD_Stamp);
	self:AddKey(prefix,"M",ADD_BlockName);
	self:AddKey(prefix,"A",DUCKNET_ACT_TRANSMIT);
	self:NewLine(prefix);
	if (not self:CodeTable(prefix,ADD_Data,0)) then return nil; end			-- Could not build output with this data
	return self:DoTransmission(prefix);
end


function DM.Net:SyncTable(prefix,ADD_BlockName,ADD_Stamp)
	if (not ADD_BlockName) then ADD_BlockName=prefix; end
	return self:Poll(prefix,ADD_Stamp,ADD_BlockName);
end



--[[  Checks if DuckNet is ready to handle your addon  ]]
function DM.Net:Idle(prefix)
	if (not self:NotCrusial(prefix)) then return nil; end;
	if (self.DB[prefix].Negotiate.HoldOff>DUCKNET_NEG_STOPPED) then return nil; end;	-- Negotiation is currently running
	return true;
end


--[[  Checks if DuckNet is ready to transmit  ]]
function DM.Net:CanTransmit(prefix)
	if (not self:NotCrusial(prefix)) then return nil; end;
	if (self.DB[prefix].Negotiate.HoldOff<DUCKNET_NEG_DONE and self.DB[prefix].Negotiate.HoldOff>DUCKNET_NEG_STOPPED) then return nil; end;		-- Negotiation is off or done
	return true;
end


--[[  Send an entire table directly (Still being designed, so please don't use this yet)  ]]
--[[  To merely update data to latest version, use SyncTable with a known table ]]
function DM.Net:SendRawTable(prefix,t)
	if (not self:CanTransmit(prefix)) then							-- Invalid or busy
		if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: DuckNet is not idle @ SendTable"); end
		return nil;
	end;
	-- Insert generic header
	self:AddKey(prefix,"M","Table");
	self:AddKey(prefix,"A",DUCKNET_ACT_TRANSMIT);
	self:NewLine(prefix);

	-- Build transmission queue
	local level=0;												-- Set root
	DM:ClearTable(self.DB[prefix].Output.Tables);
	if (not self:CodeTable(prefix,t,level)) then
		-- Error when coding the table. Handle it.
		return;
	end

	-- Footer
	self:DoTransmission(prefix);
end


--[[  Poll network for newer data  ]]
function DM.Net:Poll(prefix,stamp,marker,Tversion)
	if (not self:ClearOutput(prefix)) then
		if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: DuckNet is not idle @ Poll"); end
		return nil;
	end
	if (not stamp) then stamp=0; end
	self:AddKey(prefix,"S",stamp);
	if (marker) then self:AddKey(prefix,"M",marker); end
	if (Tversion) then self:AddKey(prefix,"T",Tversion); end
	self:AddKey(prefix,"A",DUCKNET_REQ_NEWDATA);
	self:StartNegotiation(prefix,marker,stamp);
	if (DuckMod_Present) then DuckMod_DN_Negotiation(nil,marker,stamp,0,true); DuckMod_DN_Neg_SetMyStamp(stamp); end
	return self:SingleTransmit(prefix);
end


--[[  Clear output buffer  ]]
function DM.Net:ClearOutput(prefix)
	if (not self:CanTransmit(prefix)) then					-- Invalid or busy
		if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: DuckNet is not idle @ ClearOutput"); end
		return nil;
	end;
	self.DB[prefix].Output.Buffer={ };						-- Clear table
	self.DB[prefix].Output.Line=1;							-- Start at the top
	self.DB[prefix].Output.Entry=1;							-- Start at the top
	self.DB[prefix].Output.Tables={ };						-- Clear
	return true;												-- It's cleared
end


--[[  Advance line in output buffer  ]]
function DM.Net:NewLine(prefix)
	if (not self:CanTransmit(prefix)) then					-- Invalid or busy
		if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: DuckNet is not idle @ NewLine"); end
		return nil;
	end;
	if (self.DB[prefix].Output.Buffer[self.DB[prefix].Output.Line]) then		-- Current line exists
		self.DB[prefix].Output.Line=self.DB[prefix].Output.Line+1;		-- Go to next line
	end
	return true;												-- It's cleared
end


--[[  Add a new key/action/marker  ]]
function DM.Net:AddKey(prefix,name,info)
	if (not self:CanTransmit(prefix)) then					-- Invalid or busy
		if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: DuckNet is not idle @ AddKey"); end
		return nil;
	end
	if (string.len(name)~=1) then return nil; end;
	local tp=self.DB[prefix].Output.Buffer;
	if (not tp[self.DB[prefix].Output.Line]) then
		tp[self.DB[prefix].Output.Line]="";
	end
	tp[self.DB[prefix].Output.Line]=tp[self.DB[prefix].Output.Line]..DUCKNET_COMMAND..name..info;
	return true;												-- It's cleared
end


--[[  Add a new table name  ]]
function DM.Net:AddTable(prefix,name)
	return self:AddEntry(prefix,name);
end


--[[  Add a new entry with data  ]]
function DM.Net:AddEntry(prefix,name,data,rebuild,nosplit)
	if (not self:CanTransmit(prefix)) then					-- Invalid or busy
		if (DuckNet_Debug) then DM:Chat(prefix.." DEBUG: DuckNet is not idle @ AddEntry"); end
		return nil;
	end
	if (string.len(name)<1) then return nil; end;				-- Name too short
	local tp=self.DB[prefix].Output.Buffer;
	if (not tp[self.DB[prefix].Output.Line]) then
		tp[self.DB[prefix].Output.Line]=DUCKNET_COMMAND.."E"..self.DB[prefix].Output.Entry;
		self.DB[prefix].Output.Entry=self.DB[prefix].Output.Entry+1;
	end

	-- Add it
	local lastline=tp[self.DB[prefix].Output.Line];
	local entry=self:MakeEntry(DUCKNET_ENTRY,name,data);
	tp[self.DB[prefix].Output.Line]=tp[self.DB[prefix].Output.Line]..entry;

	-- Check length. Break if need be
	local max=250-string.len(prefix);
	if (string.len(tp[self.DB[prefix].Output.Line])>max) then					-- Too long
		if (rebuild) then return nil; end											-- Currently rebuilding, data is untransmittable
		if (nosplit) then																	-- Can't split line
			self:AutoTableLinebreak(prefix,true);										-- Break and rebuild and start next line either way
			tp[self.DB[prefix].Output.Line]=tp[self.DB[prefix].Output.Line]..entry;
			if (string.len(tp[self.DB[prefix].Output.Line])>max) then return nil; end	-- Still too long - Can't rebuild
		else
			local overhead=string.len(DUCKNET_ENTRYBREAKCONTINUE);							-- Splitter overhead
			while (string.len(tp[self.DB[prefix].Output.Line])>max) do					-- Still too long
				if (not self:NewLine(prefix)) then return nil; end						-- Can't add line
				tp[self.DB[prefix].Output.Line]=tp[self.DB[prefix].Output.Line-1];	-- Copy before crop-clip
				tp[self.DB[prefix].Output.Line-1]=string.sub(tp[self.DB[prefix].Output.Line],1,max-overhead)..DUCKNET_ENTRYBREAKCONTINUE;
				local UTF=0;
				while (string.len(tp[self.DB[prefix].Output.Line-1])>max) do		-- Bleedin' charcoding...
					tp[self.DB[prefix].Output.Line-1]=string.sub(tp[self.DB[prefix].Output.Line],1,(max-overhead)-UTF)..DUCKNET_ENTRYBREAKCONTINUE;
					UTF=UTF+1;
				end
				tp[self.DB[prefix].Output.Line]=string.sub(tp[self.DB[prefix].Output.Line],((max-overhead)-UTF)+1);
			end
		end
	end
	if (rebuild) then return true; end									-- Rebuild complete
	-- At this point, any requested data has been added successfully

	-- Table book-keeping
	if (not data) then table.insert(self.DB[prefix].Output.Tables,name); end	-- Add this table

	return true;												-- It's cleared
end


--[[  Finalise and start tarnsmission  ]]
function DM.Net:DoTransmission(prefix,freeform)
	if (not freeform) then
		if (not self:NewLine(prefix)) then
			if (self.DuckNet_Debug) then DM:Chat(prefix.." DEBUG: Could not add line @ DoTransmission"); end
			return nil;
		end
		if (not self:AddKey(prefix,"A",DUCKNET_ACT_TRANSMITDONE)) then return nil; end
	end

	-- Validate buffer
	local tp=self.DB[prefix].Output.Buffer;
	local overhead=string.len(prefix)+1;
	local seq=1;
	local linebreaks=0;
	local longlines=0;
	while (tp[seq]) do
		if (string.find(tp[seq],DUCKNET_LINEBREAKnative)) then
			linebreaks=linebreaks+1;
			DM:Chat("ERROR: Linebreak in line "..seq,1,0,0);
			DM:Chat(tp[seq],1,0,0);
		end
		local total=string.len(tp[seq])+overhead;
		if (total>253) then longlines=longlines+1; DM:Chat("ERROR: Line "..seq.." length: "..total,1,0,0); end
		seq=seq+1;
	end
	if (linebreaks>0 or longlines>0) then
		self:ClearOutput(prefix);
		DM:Chat("ERROR: Transmission aborted",1,0,0);
		return true;
	end

	if (self.DB[prefix].PB) then
		self.DB[prefix].PB:SetMinMaxValues(0,self.DB[prefix].Output.Line);
		self.DB[prefix].PB:SetValue(0);
	end

	self.DB[prefix].Transmitting=DUCKNET_TRANS_START;		-- Start transmisison
	self.DB[prefix].LastOutput=nil;							-- It's there now
	return true;
end


--[[  Send the first line only  ]]
function DM.Net:SingleTransmit(prefix)
	if (not self.DB[prefix].Output.Buffer[1]) then return nil; end;			-- The line is empty
	self:Out(prefix,self.DB[prefix].Output.Buffer[1]);	-- Send it
	return true;
end

function DM.Net:GoodDatatype(datatype)
	if (datatype=="number") then return true; end
	if (datatype=="string") then return true; end
	if (datatype=="boolean") then return true; end
	return nil;
end

function DM.Net:SendSimple(prefix,name,thedata)
	if (not name) then return nil; end
	if (not thedata) then return nil; end
	if (not self:GoodDatatype(type(thedata))) then return nil; end
	if (not self:ClearOutput(prefix)) then return nil; end
	if (not self:AddKey(prefix,"A",DUCKNET_ACT_TRANSMIT)) then return nil; end
	if (not self:NewLine(prefix)) then return nil; end
	if (not self:AddEntry(prefix,name,thedata)) then return nil; end
	return self:DoTransmission(prefix);
end

-- This is a bit simplified, as it does not have support for rendering
-- more than one page at the time. This is however regarded a minor problem,
-- as rendering a page is two-pass with a single update-frame inbetween.
-- The rest of the code do however support multiple simultaneous pages.
-- Check the return value for success. "nil" is fail. "true" is victory.
-- If "nil", retry next update-frame.
function DM:Render(frame,data)
	if (DM.RU.Canvas) then return nil; end	-- Could not render at this time
	DM.RU.Canvas=frame;
	if (not frame.WnRenderData) then
		frame.WnRenderData={};
		frame.WnRenderData.Widget={};
	end
	DM.RenderWnCounter=0;
	DM.RenderWnRemaining=data;
	if (DM:Render_2(DM.RU.Canvas,DM.RenderWnRemaining)) then return true; end
	-- Fail, so do fail-stuff
	DM.RU.Canvas=nil;
	DM.RenderWnCounter=1000;
	return nil;
end

function DM:Render_2(canvas,remaining)
	DM.RenderWnCounter=DM.RenderWnCounter+1;
	canvas.WnRenderData.LockH=0;			-- Far left
DM:Chat("Rendering...",1,0,1);
	-- Do blocks
	local entryname,param,data;
--	entryname,param,data,remaining=DM.RU:PullSection(remaining);
	entryname,param,remaining=DM.RU:PullSection(remaining);
	if (entryname~="<wn>") then return; end			-- Wrong data-type waiting
	DM.RU:InitPage(canvas);	-- This will work, as the FontString widget will be set properly before a new redraw
	while (remaining~="") do
		entryname,param,data,remaining=DM.RU:PullSection(remaining);
		if (entryname=="<meta>") then
DM:Chat("Setting up meta...",1,0,1);
			local remM=data;						-- Do the meta data
			while (remM~="") do
				entryname,param,data,remM=DM.RU:PullSection(remM);
				if (entryname=="<elements>") then
					local remE=data;				-- Do the elements data
					while (remE~="") do
						entryname,param,data,remE=DM.RU:PullSection(remE);
						-- Overlay parameters
						canvas.WnRenderData.Meta["<elements>"][entryname]=DM.RU:Mix(param,canvas.WnRenderData.Meta["<elements>"][entryname]);
						DM.RU:SetInterface(canvas,entryname,canvas.WnRenderData.Meta["<elements>"][entryname].Class)
					end
				elseif (entryname=="<font>") then
					local name="Default";
					if (param.name) then name=param.name; end
					param.name=nil;
					canvas.WnRenderData.Meta.font[name]=DM:CopyTable(param,canvas.WnRenderData.Meta.font[name]);
				else
					canvas.WnRenderData.Meta[entryname]=DM:CopyTable(param,canvas.WnRenderData.Meta[entryname]);
				end
			end
		elseif (entryname=="<body>") then
DM:Chat("Rendering body...",1,0,1);
			-- Set the page standards from meta
			if (canvas.WnRenderData.Meta["<page>"]) then
				if (canvas.WnRenderData.Meta["<page>"].background) then
					local bgwidget=DM.RU:GetWidget(canvas,"<none>",canvas.WnRenderData.Meta["<elements>"]["<img>"],nil);
					bgwidget:SetDrawLayer("BACKGROUND");
					bgwidget:ClearAllPoints();
					bgwidget:SetPoint("TOPLEFT",canvas,"TOPLEFT");
					bgwidget:SetPoint("BOTTOMRIGHT",canvas,"BOTTOMRIGHT");
					local r,g,b,a=DM.RU:SplitColor(canvas.WnRenderData.Meta["<page>"].background);	-- Try to make RGBA
					if (r) then bgwidget:SetTexture(r,g,b,a);							-- Single color
					else bgwidget:SetTexture(canvas.WnRenderData.Meta["<page>"].background); end		-- Image-file
					bgwidget:Show();
				end
			end
			-- Do the contents from data
			local remB=data;						-- Do the body data
			while (remB~="") do
				entryname,param,data,remB=DM.RU:PullSection(remB);
				if (not entryname) then
					DM:Chat("Unknown tag: ?",1);
				elseif (entryname=="<set>") then
					DM.RU:SetContext(canvas,param);	-- Non-visual elements
				elseif (not canvas.WnRenderData.Meta["<elements>"][entryname]) then
					DM:Chat("Unknown tag: "..entryname,1);
				else
					-- Visual elements
					if (not canvas.WnRenderData.Meta["<elements>"][entryname]) then DM:Chat("Missing element <"..entryname..">",1); return; end
					-- * Create a widget, or retrieve an old from a stack.
					local widget=DM.RU:GetWidget(canvas,entryname,canvas.WnRenderData.Meta["<elements>"][entryname],param);
					-- * Make parameters
					local mix=DM.RU:Mix(param,canvas.WnRenderData.Meta["<elements>"][entryname]);
					-- * Set provided data
					local r,g,b,a=DM.RU:SplitColor(data);	-- Try to make an RGBA
					if (not r) then
						if (DM.RU:Flag(canvas,canvas.WnRenderData.Meta["<elements>"][entryname],"FontInstance")) then
							DM.RU:SetFont(canvas,widget,mix);
							widget:SetShadowOffset(0,0);
						end
DM:Chat(entryname);
if (not canvas.WnRenderData.Meta["<elements>"][entryname].IF.SetData) then
	DM:Chat("no setdata");
end
						widget[canvas.WnRenderData.Meta["<elements>"][entryname].IF.SetData](widget,data);	-- Simulate colon
					else
						widget[canvas.WnRenderData.Meta["<elements>"][entryname].IF.SetData](widget,r,g,b,a);	-- Simulate colon
					end
					-- * Anchor and size it
					DM.RU:SetPosition(canvas,widget,mix);
					-- * Show it
					widget:Show();
					canvas.WnRenderData.LastWidget=widget;
				end
			end
			canvas:SetHeight(canvas.WnRenderData.Bottom+1);
		else
			-- Unknown block
		end
	end
	DM.RenderWnCounter=DM.RenderWnCounter+1;
	if (DM.RenderWnCounter==3) then DM.RU.Canvas=nil; end		-- Done with this canvas
	return true;
end


-- Render Utilities
DM.RU={
	Class={
		-- "=" denotes the named class
		-- "x" denotes included entities
		HaveContainer = 0x00000001, -- x x
		UIObject      = 0x00000002, -- x x x x x
		FontInstance  = 0x00000004, -- x   x
		Region        = 0x00000008, -- x x x x x
		LayeredRegion = 0x00000010, -- x x
		FontString    = 0x00000020, -- =
		Texture       = 0x00000040, --   =
		Frame         = 0x00000080, --     x x =
		EditBox       = 0x00000100, --     =
		Button        = 0x00000200, --       =

		WNCheckBox    = 0x40000000,
		WNListBox     = 0x80000000,

		FontString_   = 0x0000003F, -- Full FontString
		Texture_      = 0x0000005B, -- Full Texture
		Frame_        = 0x0000008A, -- Full Frame
		EditBox_      = 0x0000018E, -- Full EditBox
		Button_       = 0x0000028A, -- Full Button
	},
};


-- Deviations to the default set-up of parameters are done by table overlay.
DM.RU.DefaultMeta={
	-- Create the building-blocks
	["<page>"]={
		background="#FFFFFFFF",								-- #AARRGGBB or filename
--		package="",											-- A registered package
	},
	font={			-- EXCEPTION: Not bracketed, as it is not a tag-to-entry table
		Default={
			inheritSys="GameFontNormal",	-- Inherit a system font by name
			size="14",
			color="#FF000000",				-- Black opaque
			flags="",
		},
		Medium={
			size="22",
		},
		Big={
			size="28",
		},
		Edit={
			color="#FF505050",
		},
	},
	-- Create the elements
	["<elements>"]={
		["<none>"]={							-- dummy
			Class=0,
		},
		["<text>"]={
			widget="FontString",				-- the widget
			font="Default",
			pad=3,
			Class=DM.RU.Class.FontString_,
		},
		["<h1>"]={
			widget="FontString",				-- the widget
			font="Big",
			pad=3,
			Class=DM.RU.Class.FontString_,
		},
		["<h2>"]={
			widget="FontString",				-- the widget
			font="Medium",
			pad=3,
			Class=DM.RU.Class.FontString_,
		},
		["<img>"]={
			widget="Texture",					-- the widget
			pad=3,
			Class=DM.RU.Class.Texture_,
		},
		["<edit>"]={
			widget="EditBox",					-- the widget
			inheritSys="DuckMod_EditText_01",	-- Inherit a frame
			width=200,
			height=25,
			font="Edit",
			pad=3,
			Class=DM.RU.Class.EditBox_,
		},
		["<button>"]={
			widget="Button",					-- the widget
			inheritSys="UIPanelButtonTemplate2",
			width=125,
			height=22,
			pad=3,
			Class=DM.RU.Class.Button_,
		},
--		checkbox={
--			widget="CheckBox",
--		},
--		listbox={
--			widget="WnListBox",
--			lines="10",					-- Number of visible lines
--		},
	},
};

-- Each canvas (frame) will have the following additional info:
-- canvas
--    WnRenderData
--       Widget     - Storage for widget objects
--       Meta       - Meta for this page. That is: compined default and provided.
--       LastWidget - Last rendered widget
--       Bottom     - Current end-of-page
--       WnContext  - Current page context (<set>)
--       LockH      - The current LockH while rendering

-- Since each canvas keeps its own registry, multiple rendered pages is supported.
function DM.RU:InitPage(canvas)
	canvas.WnRenderData.Meta=DM:CopyTable(DM.RU.DefaultMeta);	-- Set default meta
	canvas.WnRenderData.LastWidget=nil;				-- nil is the container frame, and true only for the first widget
	canvas.WnRenderData.Bottom=0;					-- Bottom of reendered page
	canvas.WnRenderData.WnContext={};				-- Current run-length context
	canvas.WnRenderData.LockH=0;					-- Last locked horizontal
	-- Clear all used elements
	for wEntry,wTable in pairs(canvas.WnRenderData.Widget) do     -- All widget groups
		for wwEntry,wwTable in pairs(wTable) do		-- All widgets within group
			wwTable.Used=nil;						-- New page, so all unused
			DM.RU:InitWidget(wwTable.widget);
		end
	end
DM:Chat("Setting default interfaces...",1,0,1);
	-- Set interface for all defaults
	for nElement,dElement in pairs(canvas.WnRenderData.Meta["<elements>"]) do
		self:SetInterface(canvas,nElement,dElement.Class)
	end
end

function DM.RU:InitWidget(widget)
	widget:Hide();					-- Hide all
	widget:ClearAllPoints();
	widget:SetHeight(0);
	widget:SetWidth(0);
end

function DM.RU.PageButtonClicked(button)
	if (not button) then
		DM:Chat("No button provided");
	else
		local canvas=button.WnCanvas;
		DM:Chat("Button clicked: "..button:GetName());
		if (not button.WnParam) then						-- No params in XML
			DM:Chat("No action associated with this button.");
		elseif (button.WnParam.send) then					-- param "send" supplied
			if (button.WnContext[button.WnParam.send]) then	-- It's send-data exists in the context
				local val=button.WnContext[button.WnParam.send];
				local transData={};
				local stored=nil;
				for wEntry,wTable in pairs(canvas.WnRenderData.Widget) do     -- All widget groups
					local GetMyData=canvas.WnRenderData.Meta["<elements>"][button.WnElement].IF.GetData;
					for wwEntry,wwTable in pairs(wTable) do		-- All widgets within group
						if (canvas.WnRenderData.Meta["<elements>"][wwTable.widget.WnElement].IF.GetData) then
							if (wwTable.Used and wwTable.widget.WnParam.id) then
								transData[wwTable.widget.WnParam.id]=wwTable.widget[GetMyData](wwTable.widget);
								stored=true;
							end
						end
					end
				end
					-- We've got data, so send it
				if (stored) then
if (transData.feedText) then	-- The id for the editcontrol
	DM:Chat("Data: "..transData.feedText);
end
				end
			end
		else
			DM:Chat("No known action associated with this button.");
		end
	end
end


-- The following code enables context-code, like so:
-- <set form="guild-application"/>  -- Starting form for guild application (multiple forms allowed)
-- <edit req="1"/>                  -- Assigned to form "guild-application" (requires at least one character data)
-- <button send="form"/>            -- Assigned to form "guild-application"
-- <set form="guild-log-in"/>       -- Starting form for log-in, guild application form is done
-- <edit ID="LIname"/>
-- <edit ID="LIpass"/>
-- <set form=""/>					-- No form active
-- Following this, pressing the button will collect data from all active elements
-- associated with the same context. In this case "form".
function DM.RU:SetContext(canvas,param)
	if (not param) then param={}; end
	canvas.WnRenderData.WnContext=DM.RU:Mix(param,canvas.WnRenderData.WnContext);
	for entry,eVal in pairs(canvas.WnRenderData.WnContext) do
		if (eVal=="") then canvas.WnRenderData.WnContext[entry]=nil; end		-- Remove emptied parameters
	end
end

function DM.RU:Mix(paramA,paramB)
	if (not paramA) then paramA={}; end
	local param=DM:CopyTable(paramB);		-- Make copy
	return DM:CopyTable(paramA,param);		-- Merge and return
end

-- Handle "glue" and "flow"
function DM.RU:SetPosition(canvas,widget,param)
	if (not param) then param={}; end
	local glue,flow,align,pad=param.glue,param.flow,param.align,param.pad;		-- Make copies
	local bottomtype="stack";

	widget:ClearAllPoints();

	local hSet,wSet;
	if (param.width) then
		local value=tostring(param.width);
		if (value:sub(-1)=="%") then
			value=tonumber(value:sub(1,-2));			-- Get the percentage
			value=(canvas:GetWidth()/100)*value;	-- Convert to points
		end
DM:Chat("Width: "..tonumber(value));
		widget:SetWidth(tonumber(value));				-- Set provided value directly
		wSet=true;
	end
	if (param.height) then
		widget:SetHeight(tonumber(param.height));		-- Set provided value directly
		hSet=true;
	end

	-- Check provided information
	if (not glue and not flow and not align) then align="LEFT"; end
	if (not align) then align=""; end
	if (not glue) then glue=""; end
	if (not flow) then flow=""; end
	if (not pad) then pad=0; end
	align=align:upper();
	glue=glue:upper();
	flow=flow:upper();

DM:Chat("g:"..glue.." f:"..flow.." a:"..align);

	-- Set text adjustment. "align" has presedence as it does that for positioning as well.
	local adjust;
	adjust="LEFT";		-- Since we are left-to-right readers
	if (flow=="LEFT") then adjust="RIGHT"; end
	if (flow=="DOWN") then adjust="CENTER"; end
	if (align=="LEFT" or align=="RIGHT" or align=="CENTER") then adjust=align; end

	local lockside,bottomadd="LEFT",pad;
	-- Align with canvas in stead of widget
	if (align=="LEFT" or (not canvas.WnRenderData.LastWidget and glue=="LEFT")) then
		widget:SetPoint("TOPLEFT",canvas,"TOPLEFT",0,0-(canvas.WnRenderData.Bottom+bottomadd));
		canvas.WnRenderData.LockH=0;
		adjust="LEFT";
	elseif (align=="RIGHT" or (not canvas.WnRenderData.LastWidget and glue=="RIGHT")) then
		widget:SetPoint("TOPRIGHT",canvas,"TOPRIGHT",0,0-(canvas.WnRenderData.Bottom+bottomadd));
		lockside="RIGHT";
		canvas.WnRenderData.LockH=canvas:GetWidth()-0;
		adjust="RIGHT";
	elseif (align=="CENTER" or (not canvas.WnRenderData.LastWidget and glue=="TOP")) then
		widget:SetPoint("TOP",canvas,"TOP",0,0-(canvas.WnRenderData.Bottom+bottomadd));
		lockside="CENTER";
		canvas.WnRenderData.LockH=canvas:GetWidth()/2;
		adjust="CENTER";
	elseif (glue=="LEFT") then
		if (flow=="DOWN") then widget:SetPoint("TOPLEFT",canvas.WnRenderData.LastWidget,"TOPRIGHT",pad,0); bottomtype="TOPDOWN";
		elseif (flow=="UP") then widget:SetPoint("BOTTOMLEFT",canvas.WnRenderData.LastWidget,"BOTTOMRIGHT",pad,0); bottomtype="BOTTOMUP";
		else widget:SetPoint("LEFT",canvas.WnRenderData.LastWidget,"RIGHT",pad,0); bottomtype="CENTERSIDE"; end
		adjust="LEFT";
		bottomadd=0;
	elseif (glue=="RIGHT") then
		if (flow=="DOWN") then widget:SetPoint("TOPRIGHT",canvas.WnRenderData.LastWidget,"TOPLEFT",-pad,0); bottomtype="TOPDOWN";
		elseif (flow=="UP") then widget:SetPoint("BOTTOMRIGHT",canvas.WnRenderData.LastWidget,"BOTTOMLEFT",-pad,0); bottomtype="BOTTOMUP";
		else widget:SetPoint("RIGHT",canvas.WnRenderData.LastWidget,"LEFT",-pad,0); bottomtype="CENTERSIDE"; end
		lockside="RIGHT";
		adjust="RIGHT";
		bottomadd=0;
	elseif (glue=="TOP") then
		if (flow=="DOWN") then widget:SetPoint("TOP",canvas.WnRenderData.LastWidget,"BOTTOM",0,0-pad); lockside="CENTER"; adjust="CENTER";
		elseif (flow=="LEFT") then widget:SetPoint("TOPRIGHT",canvas.WnRenderData.LastWidget,"BOTTOMRIGHT",0,0-pad); lockside="RIGHT"; adjust="RIGHT";
		else widget:SetPoint("TOPLEFT",canvas.WnRenderData.LastWidget,"BOTTOMLEFT",0,0-pad); lockside="LEFT"; adjust="LEFT"; end
	end
	if (widget.SetJustifyV) then widget:SetJustifyV("TOP"); end
	if (widget.SetJustifyH) then widget:SetJustifyH(adjust); end

	-- Expand widget if text is bigger than said area
	if (not wSet and widget.GetStringWidth) then
DM:Chat("here...");
		local _,step,_=widget:GetFont();
		local cWidth=canvas:GetWidth();
		local sWidth=widget:GetStringWidth();

		if (lockside=="LEFT") then
			canvas.WnRenderData.LockH=canvas.WnRenderData.LockH+pad;
			if (canvas.WnRenderData.LockH+sWidth>=cWidth) then widget:SetWidth(cWidth-canvas.WnRenderData.LockH);
			elseif (canvas.WnRenderData.LockH+sWidth+step>=cWidth) then widget:SetWidth(cWidth-canvas.WnRenderData.LockH-step);
			else
				--widget:SetWidth(sWidth);
			end
		elseif (lockside=="RIGHT") then
			canvas.WnRenderData.LockH=canvas.WnRenderData.LockH-pad;
			if (canvas.WnRenderData.LockH-sWidth<=0) then widget:SetWidth(canvas.WnRenderData.LockH);
			elseif (canvas.WnRenderData.LockH-(sWidth+step)<=0) then widget:SetWidth(canvas.WnRenderData.LockH-step);
			else
				--widget:SetWidth(sWidth+step);
			end
		elseif (lockside=="CENTER") then
			if (canvas.WnRenderData.LockH-(sWidth/2)<=0) then widget:SetWidth(cWidth);
			else widget:SetWidth(sWidth+step); end
		end
	end


	if (not hSet and widget.GetStringHeight) then
		widget:SetHeight(widget:GetStringHeight()+1);
	end

	-- Set new lock-side
	if (lockside=="LEFT") then
		canvas.WnRenderData.LockH=canvas.WnRenderData.LockH+widget:GetWidth();
	elseif (lockside=="RIGHT") then
		canvas.WnRenderData.LockH=canvas.WnRenderData.LockH-widget:GetWidth();
	elseif (lockside=="CENTER") then
		-- Assume continuous position, even though that may be wrong when the center is center of an element
	end

	-- Set the visually lowest point of the rendering
	canvas.WnRenderData.Bottom=canvas.WnRenderData.Bottom+bottomadd;		-- Add any vertical padding
	if (bottomtype=="stack") then
		canvas.WnRenderData.Bottom=canvas.WnRenderData.Bottom+widget:GetHeight();
	elseif (bottomtype=="TOPDOWN") then
		if (widget:GetHeight()>canvas.WnRenderData.LastWidget:GetHeight()) then
			canvas.WnRenderData.Bottom=canvas.WnRenderData.Bottom+(widget:GetHeight()-canvas.WnRenderData.LastWidget:GetHeight());
		end
	elseif (bottomtype=="CENTERSIDE") then
		if (widget:GetHeight()>canvas.WnRenderData.LastWidget:GetHeight()) then
			canvas.WnRenderData.Bottom=canvas.WnRenderData.Bottom+(widget:GetHeight()-canvas.WnRenderData.LastWidget:GetHeight())/2;
		end
	end
end

-- Find an unused widget or create a new
function DM.RU:GetWidget(canvas,element,widget,param)
	if (not param) then param={}; end
	local typename=widget.widget;
	if (not canvas.WnRenderData.Widget[typename]) then canvas.WnRenderData.Widget[typename]={}; end
	local TheName=nil;
	for wEntry,wTable in pairs(canvas.WnRenderData.Widget[typename]) do
		if (not wTable.Used) then
			TheName=wEntry;
		end
	end

	-- Create new if needed
	if (not TheName) then
		TheName="WoWnet-widget-"..DM:Random(0,1000000);		-- Fractional million
		canvas.WnRenderData.Widget[typename][TheName]={};
		if (self:Flag(canvas,widget,"HaveContainer")) then
			local creator="Create"..typename;
			canvas.WnRenderData.Widget[typename][TheName].widget=canvas[creator](canvas,TheName);	-- Simulate colon
		else
			canvas.WnRenderData.Widget[typename][TheName].widget=CreateFrame(typename,TheName,canvas,widget.inheritSys);
		end
	else
		canvas.WnRenderData.Widget[typename][TheName].widget:SetParent(canvas);			-- In case it was moved earlier
		wipe(canvas.WnRenderData.Widget[typename][TheName].widget.WnContext);				-- Clear the context for the element
		canvas.WnRenderData.Widget[typename][TheName].widget.WnContext=nil;				-- And throw it away
		if (canvas.WnRenderData.Widget[typename][TheName].widget.WnParam) then
			wipe(canvas.WnRenderData.Widget[typename][TheName].widget.WnParam);				-- Clear the parameters for the element
			canvas.WnRenderData.Widget[typename][TheName].widget.WnParam=nil;					-- And throw it away
		end
	end
	-- Set up the widget a bit
	canvas.WnRenderData.Widget[typename][TheName].widget.WnElement=element;
	if (canvas.WnRenderData.Widget[typename][TheName].widget.SetDrawLayer) then
		canvas.WnRenderData.Widget[typename][TheName].widget:SetDrawLayer("ARTWORK");
	end
	canvas.WnRenderData.Widget[typename][TheName].widget.WnContext=DM:CopyTable(canvas.WnRenderData.WnContext);	-- Insert current context in the element
	canvas.WnRenderData.Widget[typename][TheName].Used=true;
	canvas.WnRenderData.Widget[typename][TheName].widget.WnParam=DM:CopyTable(param);
	canvas.WnRenderData.Widget[typename][TheName].widget.WnCanvas=canvas;
	-- Set handlers
	if (self:Flag(canvas,widget,"Button")) then
		canvas.WnRenderData.Widget[typename][TheName].widget:SetScript("OnClick",DM.RU.PageButtonClicked);
	end

	DM.RU:InitWidget(canvas.WnRenderData.Widget[typename][TheName].widget);

	return canvas.WnRenderData.Widget[typename][TheName].widget;
end

-- This function takes a parameter table, and assumes that it inherits
-- all the way back to default.
function DM.RU:SetFont(canvas,widget,param)
	local face,height,flags="",0,"";
	if (not param) then param={}; end

	-- Start with default
	face,height,flags=self:MakeFont(widget,face,height,flags,canvas.WnRenderData.Meta.font.Default)

if (param.font) then
	DM:Chat(param.font);
end
	if (param.font and canvas.WnRenderData.Meta.font[param.font]) then			-- Font supplied and it exists
		face,height,flags=self:MakeFont(widget,face,height,flags,canvas.WnRenderData.Meta.font[param.font]);
	end
	widget:SetFont(face,tonumber(height),flags);
end

function DM.RU:MakeFont(widget,face,height,flags,font)
	if (not font) then return face,height,flags; end

	if (font.inheritSys) then										-- Inherits from game font-object
		widget:SetFontObject(font.inheritSys);						-- Set base font from Default
		face,height,flags=widget:GetFont();							-- Get the data for it
	elseif (font.inherits and DM.RU.Meta.font[font.inherits]) then	-- Inherits from some other font
		face,height,flags=DM.RU:MakeFont(face,height,flags,canvas.WnRenderData.Meta.font[font.inherits]);
	end
	if (font.file) then face=font.file; end
	if (font.size) then height=font.size; end
	if (font.flags) then flags=font.flags; end
	if (font.color) then
		local r,g,b,a=self:SplitColor(font.color);
		if (r) then widget:SetTextColor(r,g,b,a); end
	end
DM:Chat(height);
	return face,height,flags;
end

function DM.RU:SplitColor(color)
	local r,g,b,a;
	if (color:find("#",1,true)==1) then
		color=color:sub(2);
		if (color:len()==8) then		-- Standard full-resolution mode 0-255
			a=tonumber("0x"..color:sub(1,2)); if (a>0) then a=255/a; end
			r=tonumber("0x"..color:sub(3,4)); if (r>0) then r=255/r; end
			g=tonumber("0x"..color:sub(5,6)); if (g>0) then g=255/g; end
			b=tonumber("0x"..color:sub(7,8)); if (b>0) then b=255/b; end
		elseif (color:len()==4) then	-- Simple html-mode 0-15
			a=tonumber("0x"..color:sub(1,1)); if (a>0) then a=15/a; end
			r=tonumber("0x"..color:sub(2,2)); if (r>0) then r=15/r; end
			g=tonumber("0x"..color:sub(3,3)); if (g>0) then g=15/g; end
			b=tonumber("0x"..color:sub(4,4)); if (b>0) then b=15/b; end
		end
	end
	return r,g,b,a;
end

function DM.RU:SetInterface(canvas,element,class)
	if (not canvas.WnRenderData.Meta["<elements>"][element]) then return; end
--DM:Chat("IF: "..element);
	canvas.WnRenderData.Meta["<elements>"][element].Class=class;
	if (not canvas.WnRenderData.Meta["<elements>"][element].IF) then canvas.WnRenderData.Meta["<elements>"][element].IF={}; end
	-- Check for implied interfaces
	if (not bit.band(class,DM.RU.Class.UIObject)) then DM:Chat("<"..element.."> does not have UIObject",1); return; end
	if (not bit.band(class,DM.RU.Class.Region)) then DM:Chat("<"..element.."> does not have Region",1); return; end
	-- Clear the common programming interface
	canvas.WnRenderData.Meta["<elements>"][element].IF.SetData=nil;		-- No method
	canvas.WnRenderData.Meta["<elements>"][element].IF.GetData=nil;		-- No method

	-- Set the common programming interface
	if (bit.band(class,DM.RU.Class.FontInstance)==DM.RU.Class.FontInstance) then
		canvas.WnRenderData.Meta["<elements>"][element].IF.SetData="SetText";
		canvas.WnRenderData.Meta["<elements>"][element].IF.GetData="GetText";
	elseif (bit.band(class,DM.RU.Class.Button)==DM.RU.Class.Button) then
		canvas.WnRenderData.Meta["<elements>"][element].IF.SetData="SetText";
		canvas.WnRenderData.Meta["<elements>"][element].IF.GetData="GetText";
	elseif (bit.band(class,DM.RU.Class.Texture)==DM.RU.Class.Texture) then
		canvas.WnRenderData.Meta["<elements>"][element].IF.SetData="SetTexture";
	end
end

function DM.RU:Flag(canvas,widget,flag)
	if (bit.band(widget.Class,DM.RU.Class[flag])==DM.RU.Class[flag]) then
		return true;
	end
	return nil;
end



-- * The section must start at the first character
-- * Does NOT support nested same-type top-tags
-- Returns:
--   Section name: String. Contents of first tag without parameters
--     Parameters: Table. All parameters where entry is param-name and value is string value
--           Data: The entire contents between starting and ending tags
--      Remaining: The remainder of the data-block
function DM.RU:PullSection(data)
DM:Chat("PullSection...",1,0,1);
	if (not data:find("<",1,true)==1) then return; end	-- No start-tag present
	if (not data:find(">",1,true)) then return; end		-- No tag present
DM:Chat("- got tag(s)",1,0,1);
	local param=nil;									-- nil it in case there are none
	local remaining;
	local locA=data:find(">",1,true);
	local entryName=data:sub(1,locA);					-- Get tag with <
	data=data:sub(locA+1);								-- Get data without tag
	if (entryName:sub(entryName:len()-1)=="/>") then	-- Tag terminates itself
		remaining=data;									-- Copy from data that is correct from last change
		data="";										-- Set no data
	else
DM:Chat("- got tag "..entryName.." with data",1,0,1);
		local extryName;
		if (entryName:find(" ",1,true)) then			-- tag has spaces (thus also parameters)
			locA=entryName:find(" ",1,true);			-- Find first space
			extryName="</"..entryName:sub(2,locA-1)..">";	-- Make ending tag before first space
		else
			extryName="</"..entryName:sub(2);			-- Make ending tag
		end
DM:Chat("- created "..extryName.." as ending tag",1,0,1);
		locA=data:find(extryName,1,true);				-- Find the ending tag
		if (not locA) then return; end					-- ERROR: No closing tag
DM:Chat("- found "..extryName.." in data",1,0,1);
		remaining=data:sub(locA+extryName:len()+1);		-- Copy from termination and out
		data=data:sub(1,locA-1);						-- Keep only enclosed data
	end
	-- Clean up entryName (may still have parameters)
	entryName=entryName:sub(2,entryName:len()-1);		-- Cut "<" and ">"
	if (entryName:sub(-1)=="/") then					-- Self-terminating
		entryName=entryName:sub(1,-2);					-- Cut last "/"
	end
	-- Here: data OK, remaining OK, entryName /w parameters and no < and >.

	locA=entryName:find(" ",1,true);
	if (locA) then										-- tag has spaces (thus also parameters)
		param={};
		-- Seek, don't split, as spaces may be in parameters' data too.
		local work=entryName:sub(locA);					-- Cut tag
		entryName=entryName:sub(1,locA-1);				-- Cut the first space
		while (work~="") do
			local pName,pData;
			pName,pData,work=self:PullParameter(work);	-- Pull one and save the rest
			if (pName) then param[pName]=pData; end		-- Save parameter
		end
	end
	entryName="<"..strtrim(entryName)..">";		-- Cut rubbish
	data=strtrim(data);							-- Cut rubbish
	remaining=strtrim(remaining);				-- Cut rubbish
DM:Chat("<- Done: "..entryName,1,0,1);
	return entryName,param,data,remaining;		-- Return the distilled data
end

--face="/Files/Fonts/Somewhere/uglyfont.ttf" size="12" color="#AARRGGBB"
function DM.RU:PullParameter(data)
	data=strtrim(data);
	local loc=data:find("=\"",1,true);
	if (not loc) then return nil,nil,""; end
	local param=data:sub(1,loc-1);
	data=data:sub(loc+2);
	loc=data:find("\"",1,true);
	local value=data:sub(1,loc-1);
	data=data:sub(loc+1);
	data=strtrim(data);
	return param:lower(),value,data;
end


function DM:Init()
	DM.Net:Init();
end

--LeaveChannelByName("WoWnet");


end		-- if (not DuckMod[TV]) then
