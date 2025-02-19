Type TGENNAME
	Field Count%
	Field Array$[128]
	Field Snd%[128*3]
	Field ind%
End Type

Function CreateGenName.TGenName(size%)
	gn.TGenName=New TGenName
	gn\Count	=	Size
	Repeat
		index=index+1
		Size=Size/2
		If Size=1 Exit
	Forever
	gn\ind		=	index
	Return gn
End Function

Function FillGenName(gn.TGenName,Array$[128],Start%=0,Count%=128)
	For n= Start To Start+Count-1
		gn\Array[n]=Array[n]
	Next
End Function

Function GenerateName$(x%,y%,Sysnum% , Gn.TGenName)
	Base=	Gn\ind
	x	=	x+Sysnum
	y	=	y+x
    x	=	x Shl(Base-2)
    x	=	x+y
    y	=	y Shl(Base)
    y	=	y+x
    y	=	y Shl(Base-1)
    x	=	x Shl(Sysnum)
    x	=	x+y
	x_	=	x /Gn\Count And (Gn\Count-1)
	Dest$=	Gn\Array[x_]
    x	=	x Shr(Base)
	x_	=	x /Gn\Count And (Gn\Count-1)
	Dest=	Dest+Gn\Array[x_]
    x	=	x Shr(Base)
	x_	=	x /Gn\Count And (Gn\Count-1)
	Dest=	Dest+Gn\Array[x_]
	char$=	Left(Dest,1)
	If Asc(Char)>=Asc("a") And Asc(Char)<=Asc("z")
		Maj$=	Chr(Asc(char)+Asc("A")-Asc("a"))
		Dest=	Maj+Right(Dest,Len(Dest)-1)
	EndIf
	Return dest
End Function



Type User
	Field name$
	Field planet$
End Type

Graphics 800,800,0,2
	; Create layer for Name Generator ( Planet Layer )
		GENPlanetLayer.TGenName	=	CreateGenName(64)

	; Create layer for Name Generator ( Zorg Layer )
		GENZorgLayer.TGenName	=	CreateGenName(16)

	; Fill temp array
		Local Array$[128]
		Restore PlanetName
		For n = 0 To 63
			Read Array[n]
		Next

	; Past Temp Array to NameGen
		FillGenName(GENPlanetLayer,Array,0,64)

		Restore ZorgName
		For n = 0 To 16 : Read Array[n] : Next
		FillGenName(GENZorgLayer,Array,0,16)

	; Draw Names
	For x= 0 To 2
		For y = 0 To 3
			Color 200,200,100
			Text x*260,y*200+00,"Planet    : "+GenerateName(x,y,8,GENPlanetLayer)
			Color 200,150,100
			Text x*260,y*200+15,"Habitants : "
			i=0
			feed=x*32+y
			For a=0 To 2 For b= 0 To 1
				Text x*260+20,y*200+35+i*15,GenerateName(x+a,y+b+feed,8,GENZorgLayer)
				i=i+1
			Next Next
		Next
	Next

Flip
WaitKey
End

.ZorgName
Data "ra" , "kil" , "so", "'n"
Data "ca", "gwa", "a", "moh"
Data "su" , "o" , "ya"  , "ar"
Data "lo" , "lip" , "ren" , "kal"

.PlanetName
Data "en" , "la" , "can", "be"
Data "and", "phi", "eth", "ol"
Data "ve" , "ho" , "a"  , "lia"
Data "an" , "ar" , "ur" , "mi"
Data "in" , "ti" , "ana" , "so"
Data "ed" , "ess", "ex" , "io"
Data "ce" , "ze" , "fa" , "ay"
Data "wa" , "ida" , "ack", "gre"
Data "atu" , "ara" , "tak", "ran"
Data "roi" , "ila" , "reb", "maar"
Data "urn" , "pit" , "plu", "ton"
Data "kle" , "éon" , "atu", "pla"
Data "aby" , "aris" , "gos" , "hos" 
Data "dos" , "lon" , "ava" , "ago" 
Data "stis" , "éria" , "mak" , "ora" 
Data "lla" , "olis" , "una" , "heb" 
