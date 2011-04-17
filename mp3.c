#include <stdlib.h>
#include <string.h>
#include <stdio.h>

char* genres[];

struct Field{
  char* name;
  char* value;
  int size;
};

typedef enum {Tag,Title,Artist,Album,Year,Comment,Track,Genre, NUMTYPES} FieldType;
char data[128];
struct Field fields[] = {
  {"TAG",     data,     3},
  {"Title",  (data+3),  30},
  {"Artist", (data+33), 30},
  {"Album",  (data+63), 30},
  {"YEAR",   (data+93), 4},
  {"Comment",(data+97), 30},
  {"Track",  (data+127),1},
  {"Genre",  (data+128),1}
};



int check_validity(char* c){
  return (c[0] == 'T') & (c[1] == 'A') & (c[2] == 'G');
}

void print_sfield(char* name, char* s, int n){
  printf("%s=",name);
  for(int i = 0; i < n && s[i]; i++)
    printf("%c",s[i]);
}

void print_ifield(char* n, int i){
  printf("%s=%d",n,i);
}
void print_field(struct Field* field){
  if(field->size > 1)
    print_sfield(field->name,field->value,field->size);
  else
    print_ifield(field->name,(int)(*field->value));
  puts("");
}

char escape_chars(char c){
  switch(c){
  case 'n': return 'n';
  case 't': return '\t';
  case 'v': return '\v';
  case 'b': return '\b';
  case 'r': return '\r';
  case 'f': return '\f';
    /* 'a': return '\a'; */
    /* 'x': return '\x'; */
    /* 'u': return '\u'; */
    /* 'o': return '\o'; */
    /* 'o': return '\o'; */
  case '\\': return '\\';
  }
  return -1; 
}

int geti(char c){
  switch(c){
  case 'a': return Artist;
  case 't': return Title;
  case 'l': return Album;
  case 'n': return Track;
  case 'y': return Year;
  case 'g': return Genre;
  case 'c': return Comment;
  }
 return -1;
}

void printc(char c){
  printf("%c",c);
};

void print_fields(char* control_str,struct Field field[]){
  for(char *str = control_str; *str; str++)
    if(str[0] == '%')
      printf("%s",field[geti(++str[0])].value);
    else if(str[0] == '\\')
      printc(escape_chars(++str[0]));
    else
      printc(str[0]);
}

struct Field *id3v1 (char* file){
    FILE* input = fopen(file,"r");
    fseek(input,-128,SEEK_END);
    fread(fields[Tag].value,1,128,input);
    int v = check_validity(data);
    
    //set see if track is value, point to the proper genre
    if(!fields[Comment].value[28]) fields[Track].value = NULL;
    fields[Genre].value = genres[*fields[Genre].value];

    return v ? fields : NULL;
}

void usage(){
  puts("help will follow shortly");
};

int main (int argc, char** argv){
  char* defalt = "Title=%t\\tArtist=%a\\tAlbum=%l\\tYear%y\\Comment=%c";
  if(argc == 1) usage();
  else {
    int j = 1;
    if(strcmp("-p",argv[1]) == 0){
      defalt = argv[2];
      j = 3;
    }
    for(int i = j; i < argc; i++)
      print_fields(defalt,id3v1(argv[i]));
  }
  return 0;
}


/* f,Filename without the path [string] */
/* F,"Filename with the path [string]",1 */
/* k,"File size in KB [integer]",1 */
/* a,"Artist [string]",1 */
/* c     Comment [string] */
/* g     Musical genre [string] */
/* G     Musical genre number [integer] */
/* l     Album name [string] */
/* n     Track [integer] */
/* t     Track Title [string] */
/* y     Year [string] */
/* C     Copyright flag [string] */
/* e     Emphasis [string] */
/* E     CRC Error protection [string] */
/* L     MPEG Layer [string] */
/* O     Original material flag [string] */
/* o     Stereo/mono mode [string] */
/* p     Padding [string] */
/* v     MPEG Version [float] */
/* u     Number of good audio frames [integer] */
/* b     Number of corrupt audio frames [integer] */
/* Q     Sampling frequency in Hz [integer] */
/* q     Sampling frequency in kHz [integer] */
/* r     Bit  Rate  in  kbps  (type  and meaning affected by -r */
/*       option) */
/* m     Playing time: minutes only [integer] */
/* s     Playing time: seconds only [integer] (usually used  in */
/*       conjunction with %m) */
/* S     Total playing time in seconds [integer] */
/* %     A single percent sign */
/**


   NEED to support
   "info-artist=%a\\n"
   "info-title=%t\\n"
   "info-album=%l\\n"
   "info-tracknumber=%n\\n"
   "info-year=%y\\n"
   "info-genre=%g\\n"
   "info-note=%c\\n"
   "info-playing-time=%S\\n"))

   a artist
   t title
   l album
   n track
   y year
   g genre
   c comment
   S --not yet

**/

char *genres[] = {
  "Blues", "Classic Rock", "Country", "Dance", "Disco", "Funk","Grunge",
  "Hip-Hop", "Jazz", "Metal", "New Age", "Oldies","Other", "Pop", "R&B",
  "Rap", "Reggae", "Rock", "Techno","Industrial", "Alternative", "Ska",
  "Death Metal", "Pranks","Soundtrack", "Euro-Techno", "Ambient", "Trip-Hop",
  "Vocal","Jazz+Funk", "Fusion", "Trance", "Classical", "Instrumental",
  "Acid", "House", "Game", "Sound Clip", "Gospel", "Noise", "Alternative Rock",
  "Bass", "Soul", "Punk", "Space", "Meditative","Instrumental Pop",
  "Instrumental Rock", "Ethnic", "Gothic","Darkwave", "Techno-Industrial",
  "Electronic", "Pop-Folk","Eurodance", "Dream", "Southern Rock", "Comedy",
  "Cult", "Gangsta", "Top 40", "Christian Rap", "Pop/Funk", "Jungle",
  "Native US", "Cabaret", "New Wave", "Psychadelic", "Rave", "Showtunes",
  "Trailer", "Lo-Fi", "Tribal", "Acid Punk", "Acid Jazz", "Polka", "Retro",
  "Musical", "Rock & Roll", "Hard Rock", "Folk", "Folk-Rock","National Folk",
  "Swing", "Fast Fusion", "Bebob", "Latin", "Revival", "Celtic", "Bluegrass",
  "Avantgarde", "Gothic Rock", "Progressive Rock", "Psychedelic Rock",
  "Symphonic Rock", "Slow Rock", "Big Band", "Chorus","Easy Listening",
  "Acoustic", "Humour", "Speech", "Chanson", "Opera","Chamber Music", "Sonata",
  "Symphony", "Booty Bass", "Primus", "Porn Groove","Satire", "Slow Jam", "Club",
  "Tango", "Samba", "Folklore", "Ballad","Power Ballad", "Rhythmic Soul",
  "Freestyle", "Duet", "Punk Rock","Drum Solo", "Acapella", "Euro-House",
  "Dance Hall", "Goa", "Drum & Bass", "Club - House", "Hardcore", "Terror",
  "Indie", "BritPop", "Negerpunk","Polsk Punk", "Beat", "Christian Gangsta Rap",
  "Heavy Metal", "Black Metal","Crossover", "Contemporary Christian",
  "Christian Rock", "Merengue", "Salsa","Thrash Metal", "Anime", "JPop",
  "Synthpop", "Unknown","Unknown","Unknown", // 150
  // 105 unknowns 7 per line 15 lines + 3
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
  "Unknown","Unknown","Unknown","Unknown","Unknown","Unknown","Unknown"};
  
