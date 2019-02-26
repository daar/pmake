(*
 * Library: crc16
 * File:    crc16.pas
 * Huge parts based on libcrc sourcecode
 * Orginal Author:  Lammert Bies (libcrc in c)
 * 
 * Pascal version Author: Thaddy de Koning (Pascal translation and additional code)
 *
 * This file is licensed under the MIT License as stated below
 *
 * Original C version Copyright (c) 1999-2016 Lammert Bies
 * Pascal version Copyright (c) 2017 Thaddy de Koning
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

unit crc16;
{$mode objfpc}
interface
const
{ Polynomials }
  CRC_POLY_16 = $A001;    
  CRC_POLY_CCITT = $1021;    
  CRC_POLY_DNP = $A6BC;    
  CRC_POLY_KERMIT = $8408;    
  CRC_POLY_SICK = $8005;  
    
{ Start values }  
  CRC_START_16 = $0000;    
  CRC_START_MODBUS = $FFFF;    
  CRC_START_XMODEM = $0000;    
  CRC_START_CCITT_1D0F = $1D0F;    
  CRC_START_CCITT_FFFF = $FFFF;    
  CRC_START_KERMIT = $0000;    
  CRC_START_SICK = $0000;    
  CRC_START_DNP = $0000;    
  
{$push}{$J+}
  crc_tab16_init:Boolean = true;
  crc_tab16: array[0..255] of word =(
  $0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241, 
  $C601, $06C0, $0780, $C741, $0500, $C5C1, $C481, $0440, 
  $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1, $CE81, $0E40, 
  $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841, 
  $D801, $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40, 
  $1E00, $DEC1, $DF81, $1F40, $DD01, $1DC0, $1C80, $DC41, 
  $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680, $D641, 
  $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040, 
  $F001, $30C0, $3180, $F141, $3300, $F3C1, $F281, $3240, 
  $3600, $F6C1, $F781, $3740, $F501, $35C0, $3480, $F441, 
  $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41, 
  $FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840, 
  $2800, $E8C1, $E981, $2940, $EB01, $2BC0, $2A80, $EA41, 
  $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1, $EC81, $2C40, 
  $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640, 
  $2200, $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041, 
  $A001, $60C0, $6180, $A141, $6300, $A3C1, $A281, $6240, 
  $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480, $A441, 
  $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41, 
  $AA01, $6AC0, $6B80, $AB41, $6900, $A9C1, $A881, $6840, 
  $7800, $B8C1, $B981, $7940, $BB01, $7BC0, $7A80, $BA41, 
  $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40, 
  $B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640, 
  $7200, $B2C1, $B381, $7340, $B101, $71C0, $7080, $B041, 
  $5000, $90C1, $9181, $5140, $9301, $53C0, $5280, $9241, 
  $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440, 
  $9C01, $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40, 
  $5A00, $9AC1, $9B81, $5B40, $9901, $59C0, $5880, $9841, 
  $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81, $4A40, 
  $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41, 
  $4400, $84C1, $8581, $4540, $8701, $47C0, $4680, $8641, 
  $8201, $42C0, $4380, $8341, $4100, $81C1, $8081, $4040 );
  
  crc_tabccitt_init:boolean = true;
  crc_tabccitt:array[0..255] of word = (
  $0000, $1021, $2042, $3063, $4084, $50A5, $60C6, $70E7, 
  $8108, $9129, $A14A, $B16B, $C18C, $D1AD, $E1CE, $F1EF, 
  $1231, $0210, $3273, $2252, $52B5, $4294, $72F7, $62D6, 
  $9339, $8318, $B37B, $A35A, $D3BD, $C39C, $F3FF, $E3DE, 
  $2462, $3443, $0420, $1401, $64E6, $74C7, $44A4, $5485, 
  $A56A, $B54B, $8528, $9509, $E5EE, $F5CF, $C5AC, $D58D, 
  $3653, $2672, $1611, $0630, $76D7, $66F6, $5695, $46B4, 
  $B75B, $A77A, $9719, $8738, $F7DF, $E7FE, $D79D, $C7BC, 
  $48C4, $58E5, $6886, $78A7, $0840, $1861, $2802, $3823, 
  $C9CC, $D9ED, $E98E, $F9AF, $8948, $9969, $A90A, $B92B, 
  $5AF5, $4AD4, $7AB7, $6A96, $1A71, $0A50, $3A33, $2A12, 
  $DBFD, $CBDC, $FBBF, $EB9E, $9B79, $8B58, $BB3B, $AB1A, 
  $6CA6, $7C87, $4CE4, $5CC5, $2C22, $3C03, $0C60, $1C41, 
  $EDAE, $FD8F, $CDEC, $DDCD, $AD2A, $BD0B, $8D68, $9D49, 
  $7E97, $6EB6, $5ED5, $4EF4, $3E13, $2E32, $1E51, $0E70, 
  $FF9F, $EFBE, $DFDD, $CFFC, $BF1B, $AF3A, $9F59, $8F78, 
  $9188, $81A9, $B1CA, $A1EB, $D10C, $C12D, $F14E, $E16F, 
  $1080, $00A1, $30C2, $20E3, $5004, $4025, $7046, $6067, 
  $83B9, $9398, $A3FB, $B3DA, $C33D, $D31C, $E37F, $F35E, 
  $02B1, $1290, $22F3, $32D2, $4235, $5214, $6277, $7256, 
  $B5EA, $A5CB, $95A8, $8589, $F56E, $E54F, $D52C, $C50D, 
  $34E2, $24C3, $14A0, $0481, $7466, $6447, $5424, $4405, 
  $A7DB, $B7FA, $8799, $97B8, $E75F, $F77E, $C71D, $D73C, 
  $26D3, $36F2, $0691, $16B0, $6657, $7676, $4615, $5634, 
  $D94C, $C96D, $F90E, $E92F, $99C8, $89E9, $B98A, $A9AB, 
  $5844, $4865, $7806, $6827, $18C0, $08E1, $3882, $28A3, 
  $CB7D, $DB5C, $EB3F, $FB1E, $8BF9, $9BD8, $ABBB, $BB9A, 
  $4A75, $5A54, $6A37, $7A16, $0AF1, $1AD0, $2AB3, $3A92, 
  $FD2E, $ED0F, $DD6C, $CD4D, $BDAA, $AD8B, $9DE8, $8DC9, 
  $7C26, $6C07, $5C64, $4C45, $3CA2, $2C83, $1CE0, $0CC1, 
  $EF1F, $FF3E, $CF5D, $DF7C, $AF9B, $BFBA, $8FD9, $9FF8, 
  $6E17, $7E36, $4E55, $5E74, $2E93, $3EB2, $0ED1, $1EF0);

crc_tabkermit_init:boolean = true;
var
crc_tabkermit:array[0..255] of word = (
  $0000, $1189, $2312, $329B, $4624, $57AD, $6536, $74BF, 
  $8C48, $9DC1, $AF5A, $BED3, $CA6C, $DBE5, $E97E, $F8F7, 
  $1081, $0108, $3393, $221A, $56A5, $472C, $75B7, $643E, 
  $9CC9, $8D40, $BFDB, $AE52, $DAED, $CB64, $F9FF, $E876, 
  $2102, $308B, $0210, $1399, $6726, $76AF, $4434, $55BD, 
  $AD4A, $BCC3, $8E58, $9FD1, $EB6E, $FAE7, $C87C, $D9F5, 
  $3183, $200A, $1291, $0318, $77A7, $662E, $54B5, $453C, 
  $BDCB, $AC42, $9ED9, $8F50, $FBEF, $EA66, $D8FD, $C974, 
  $4204, $538D, $6116, $709F, $0420, $15A9, $2732, $36BB, 
  $CE4C, $DFC5, $ED5E, $FCD7, $8868, $99E1, $AB7A, $BAF3, 
  $5285, $430C, $7197, $601E, $14A1, $0528, $37B3, $263A, 
  $DECD, $CF44, $FDDF, $EC56, $98E9, $8960, $BBFB, $AA72, 
  $6306, $728F, $4014, $519D, $2522, $34AB, $0630, $17B9, 
  $EF4E, $FEC7, $CC5C, $DDD5, $A96A, $B8E3, $8A78, $9BF1, 
  $7387, $620E, $5095, $411C, $35A3, $242A, $16B1, $0738, 
  $FFCF, $EE46, $DCDD, $CD54, $B9EB, $A862, $9AF9, $8B70, 
  $8408, $9581, $A71A, $B693, $C22C, $D3A5, $E13E, $F0B7, 
  $0840, $19C9, $2B52, $3ADB, $4E64, $5FED, $6D76, $7CFF, 
  $9489, $8500, $B79B, $A612, $D2AD, $C324, $F1BF, $E036, 
  $18C1, $0948, $3BD3, $2A5A, $5EE5, $4F6C, $7DF7, $6C7E, 
  $A50A, $B483, $8618, $9791, $E32E, $F2A7, $C03C, $D1B5, 
  $2942, $38CB, $0A50, $1BD9, $6F66, $7EEF, $4C74, $5DFD, 
  $B58B, $A402, $9699, $8710, $F3AF, $E226, $D0BD, $C134, 
  $39C3, $284A, $1AD1, $0B58, $7FE7, $6E6E, $5CF5, $4D7C, 
  $C60C, $D785, $E51E, $F497, $8028, $91A1, $A33A, $B2B3, 
  $4A44, $5BCD, $6956, $78DF, $0C60, $1DE9, $2F72, $3EFB, 
  $D68D, $C704, $F59F, $E416, $90A9, $8120, $B3BB, $A232, 
  $5AC5, $4B4C, $79D7, $685E, $1CE1, $0D68, $3FF3, $2E7A, 
  $E70E, $F687, $C41C, $D595, $A12A, $B0A3, $8238, $93B1, 
  $6B46, $7ACF, $4854, $59DD, $2D62, $3CEB, $0E70, $1FF9, 
  $F78F, $E606, $D49D, $C514, $B1AB, $A022, $92B9, $8330, 
  $7BC7, $6A4E, $58D5, $495C, $3DE3, $2C6A, $1EF1, $0F78);

crc_tabdnp_init:Boolean = true;
crc_tabdnp:array[0..255] of word =(
  $0000, $365E, $6CBC, $5AE2, $D978, $EF26, $B5C4, $839A, 
  $FF89, $C9D7, $9335, $A56B, $26F1, $10AF, $4A4D, $7C13, 
  $B26B, $8435, $DED7, $E889, $6B13, $5D4D, $07AF, $31F1, 
  $4DE2, $7BBC, $215E, $1700, $949A, $A2C4, $F826, $CE78, 
  $29AF, $1FF1, $4513, $734D, $F0D7, $C689, $9C6B, $AA35, 
  $D626, $E078, $BA9A, $8CC4, $0F5E, $3900, $63E2, $55BC, 
  $9BC4, $AD9A, $F778, $C126, $42BC, $74E2, $2E00, $185E, 
  $644D, $5213, $08F1, $3EAF, $BD35, $8B6B, $D189, $E7D7, 
  $535E, $6500, $3FE2, $09BC, $8A26, $BC78, $E69A, $D0C4, 
  $ACD7, $9A89, $C06B, $F635, $75AF, $43F1, $1913, $2F4D, 
  $E135, $D76B, $8D89, $BBD7, $384D, $0E13, $54F1, $62AF, 
  $1EBC, $28E2, $7200, $445E, $C7C4, $F19A, $AB78, $9D26, 
  $7AF1, $4CAF, $164D, $2013, $A389, $95D7, $CF35, $F96B, 
  $8578, $B326, $E9C4, $DF9A, $5C00, $6A5E, $30BC, $06E2, 
  $C89A, $FEC4, $A426, $9278, $11E2, $27BC, $7D5E, $4B00, 
  $3713, $014D, $5BAF, $6DF1, $EE6B, $D835, $82D7, $B489, 
  $A6BC, $90E2, $CA00, $FC5E, $7FC4, $499A, $1378, $2526, 
  $5935, $6F6B, $3589, $03D7, $804D, $B613, $ECF1, $DAAF, 
  $14D7, $2289, $786B, $4E35, $CDAF, $FBF1, $A113, $974D, 
  $EB5E, $DD00, $87E2, $B1BC, $3226, $0478, $5E9A, $68C4, 
  $8F13, $B94D, $E3AF, $D5F1, $566B, $6035, $3AD7, $0C89, 
  $709A, $46C4, $1C26, $2A78, $A9E2, $9FBC, $C55E, $F300, 
  $3D78, $0B26, $51C4, $679A, $E400, $D25E, $88BC, $BEE2, 
  $C2F1, $F4AF, $AE4D, $9813, $1B89, $2DD7, $7735, $416B, 
  $F5E2, $C3BC, $995E, $AF00, $2C9A, $1AC4, $4026, $7678, 
  $0A6B, $3C35, $66D7, $5089, $D313, $E54D, $BFAF, $89F1, 
  $4789, $71D7, $2B35, $1D6B, $9EF1, $A8AF, $F24D, $C413, 
  $B800, $8E5E, $D4BC, $E2E2, $6178, $5726, $0DC4, $3B9A, 
  $DC4D, $EA13, $B0F1, $86AF, $0535, $336B, $6989, $5FD7, 
  $23C4, $159A, $4F78, $7926, $FABC, $CCE2, $9600, $A05E, 
  $6E26, $5878, $029A, $34C4, $B75E, $8100, $DBE2, $EDBC, 
  $91AF, $A7F1, $FD13, $CB4D, $48D7, $7E89, $246B, $1235);
{$pop}

{ Array initializations for different CRC's }
procedure init_crc16_tab; 
procedure init_crcccitt_tab; 
procedure init_crckermit_tab;
procedure init_crcdnp_tab;

{ CRC16 implementations }
function crc_16( const input_str:Pbyte;num_bytes:integer ):word;inline;
function crc_modbus( const input_str:Pbyte; num_bytes:integer):word;inline; 
function crc_ccitt_generic( const input_str:PByte; num_bytes:integer;start_value:word ):word;inline;
function crc_xmodem( const input_str:PByte;num_bytes:integer):word;inline;
function crc_ccitt_1d0f( const input_str:PByte;num_bytes:integer):word;inline;
function crc_ccitt_ffff( const input_str:PByte;num_bytes:integer):word;inline;
function crc_kermit( const input_str:PByte; num_bytes:integer ):word;inline;
function crc_dnp( const input_str:PByte; num_bytes:Integer):word; 
function crc_sick( const input_str:PByte; num_bytes:integer ):word; 

{ Update functions }
function update_crc_ccitt( crc:word;c:byte ):word; inline; 
function update_crc_kermit( crc:word; c:byte ):word;
function update_crc_dnp(crc:word; c:byte ):word;inline; 
function update_crc_sick(crc:word; c,prev_byte:Byte ):word;inline;

implementation
  
procedure init_crc16_tab; 
var
 i,j,crc,c:word;
begin
  for i := 0 to 255 do
  begin 
	crc := 0;
	c   := i;
	for j := 0 to 7 do 
	begin
	  if  (crc xor c) and $0001  > 0 then 
        crc := ( crc >> 1 ) xor CRC_POLY_16
	  else                      
	    crc :=   crc >> 1;
	  c := c >> 1;
    end;
	crc_tab16[i] := crc;
  end;
  crc_tab16_init := true;
end; 

procedure init_crcccitt_tab; 
var
 i,j,crc,c:word;
begin
  for i := 0 to 255 do
  begin
	crc := 0;
	c   := i << 8;
	for j :=0 to 7 do
	begin
	  if (crc xor c) and $8000 <> 0 then 
	    crc := ( crc << 1 ) xor CRC_POLY_CCITT
	  else                      
	    crc := crc << 1;
      c := c << 1;
	end;
	crc_tabccitt[i] := crc;
  end;
  crc_tabccitt_init := true;
end; 

procedure init_crckermit_tab;
var
  i,j,crc,c:word;
begin
  for i:=0 to 255 do 
  begin
    crc := 0;
	c   := i;
	for j := 0 to 7 do 
	begin
	  if ( (crc xor c) and $0001 ) <> 0 then 
	    crc := ( crc >> 1 ) xor CRC_POLY_KERMIT
	  else                      
	    crc := crc >> 1;
	  c := c >> 1;
	end;
    crc_tabkermit[i] := crc;
  end;
  crc_tabkermit_init := true;
end;

procedure init_crcdnp_tab;
var
  i,j,crc,c:word;
begin
  for i := 0 to 255 do 
  begin
    crc := 0;
	c := i;
	for j :=0 to 7 do 
	begin
      if ( (crc xor c) and $0001 ) <> 0 then 
        crc := ( crc >> 1 ) xor CRC_POLY_DNP
	  else                      
	    crc := crc >> 1;
	  c := c >> 1;
	end;
    crc_tabdnp[i] := crc;
  end;
  crc_tabdnp_init := true;
end;

function crc_16( const input_str:Pbyte;num_bytes:integer ):word;
var
  ptr:PByte;
  a:integer;
begin
	if not crc_tab16_init then init_crc16_tab;
	result := CRC_START_16;
	ptr := input_str;
	if ptr <> nil then for a :=0 to pred(num_bytes) do
    begin	
	  Result := (Result >> 8) xor crc_tab16[ (Result xor ptr^) and $00FF ];
	  inc(ptr);
	end;
end;

function crc_modbus( const input_str:Pbyte; num_bytes:integer):word; 
var
  ptr:PByte;
  a:integer;
begin
  if not crc_tab16_init then init_crc16_tab;
  result := CRC_START_MODBUS;
  ptr := input_str;
  if ptr <> nil then for a := 0 to pred(num_bytes) do
  begin
	result := (result >> 8) xor crc_tab16[ (result xor ptr^) and $00FF ];
	inc(ptr);
  end;
end;

function crc_ccitt_generic( const input_str:PByte; num_bytes:integer;start_value:word ):word;
var
 ptr:PByte;
 a:integer;
begin
  if not crc_tabccitt_init then init_crcccitt_tab;
  Result := start_value;
  ptr := input_str;
  if ptr <> nil then for  a := 0 to pred(num_bytes) do 
  begin 
    Result := (Result << 8) xor crc_tabccitt[ ((Result >> 8) xor ptr^) and $00FF ];
	inc(ptr);
  end;
end;

function update_crc_ccitt( crc:word;c:byte ):word; inline; 
begin
  if not crc_tabccitt_init then init_crcccitt_tab;
  result := (crc << 8) xor crc_tabccitt[ ((crc >> 8) xor word(c)) and $00FF ];
end;

function crc_xmodem( const input_str:PByte;num_bytes:integer):word;inline;
begin
 result:= crc_ccitt_generic( input_str, num_bytes, CRC_START_XMODEM );
end;

function crc_ccitt_1d0f( const input_str:PByte;num_bytes:integer):word;inline;
begin
 result:= crc_ccitt_generic( input_str, num_bytes, CRC_START_CCITT_1D0F );
end;

function crc_ccitt_ffff( const input_str:PByte;num_bytes:integer):word;inline;
begin
 result:= crc_ccitt_generic( input_str, num_bytes, CRC_START_CCITT_FFFF );
end;

function crc_kermit( const input_str:PByte; num_bytes:integer ):word;
var
  crc,low_byte,high_byte:word;
  ptr:PByte;
  a:integer;
begin
  if not crc_tabkermit_init then init_crckermit_tab;
  crc := CRC_START_KERMIT;
  ptr := input_str;
  if ptr <> nil then for a := 0 to pred(num_bytes) do
  begin
    crc := (crc >> 8) xor crc_tabkermit[ (crc xor word(ptr^)) and $00FF ];
    inc(ptr);
  end;
  low_byte  := (crc and $ff00) >> 8;
  high_byte := (crc and $00ff) << 8;
  crc := low_byte or high_byte;
  Result := crc;
end;

function update_crc_kermit( crc:word; c:byte ):word;
begin
  if not crc_tabkermit_init then  init_crckermit_tab;
  Result := (crc >> 8) xor crc_tabkermit[ (crc xor dword(c)) and $00FF ];
end;

function crc_dnp( const input_str:PByte; num_bytes:Integer):word; 
var
  low_byte,high_byte:word;
  ptr:PByte;
  a:integer;
begin
  if not crc_tabdnp_init then init_crcdnp_tab;
  Result := CRC_START_DNP;
  ptr := input_str;
  if ptr <> nil then for a :=0 to pred(num_bytes) do
  begin
    Result := (Result >> 8) xor crc_tabdnp[ (Result xor ptr^) and $00FF ];
    inc(ptr);
  end;
  Result := not Result;
  low_byte := (Result and $ff00) >> 8;
  high_byte := (Result and $00ff) << 8;
  Result := low_byte or high_byte;
end;

function update_crc_dnp(crc:word; c:byte ):word;inline; 
begin
  if not crc_tabdnp_init then init_crcdnp_tab;
  result := (crc >> 8) xor crc_tabdnp[ (crc xor dword(c)) and $00FF ];
end;

function crc_sick( const input_str:PByte; num_bytes:integer ):word; 
var
  low_byte,high_byte,short_c,short_p:word;
  ptr:PByte;
  a:integer;
begin
  Result := CRC_START_SICK;
  ptr:= input_str;
  short_p := 0;
  if ptr <> nil then for a := 0 to pred(num_bytes) do
  begin
    short_c := $00FF and word(ptr^);
    if ( Result and $8000 ) <> 0 then 
      Result := ( Result << 1 ) xor CRC_POLY_SICK      
	else                
	  Result := Result << 1;
	Result := Result xor ( short_c or short_p );
	short_p := short_c << 8;
    inc(ptr);
  end;

  low_byte  := (Result and $FF00) >> 8;
  high_byte := (Result and $00FF) << 8;
  Result := low_byte or high_byte;
end;

function update_crc_sick(crc:word; c,prev_byte:Byte ):word;inline;
var
  short_c,short_p:word;
begin
  short_c := $00FF and dword(c);
  short_p := ( $00FF and dword(prev_byte) ) << 8;
  if ( crc and $8000 ) <> 0 then 
    crc := ( crc << 1 ) xor CRC_POLY_SICK
  else                
    crc :=   crc << 1;
  crc := crc xor ( short_c or short_p );
  result := crc;
end;


end.
