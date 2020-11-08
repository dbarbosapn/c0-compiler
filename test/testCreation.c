#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int bufferSize = 1024;
char *buffer;

char* startBuffer(char *str);
char* readSomething(FILE *rd, char *something);
void readEmpty(FILE *rd);
char* readLine(FILE *rd);
void writeSomething(FILE *wr, char *str, char *not, char *sub);
void readPropName(FILE *rd, FILE *wr);
void readProp(FILE *rd, FILE *wr);
void readSol(FILE *rd, FILE *wr);


void startFile(FILE *wr);
void endFile(FILE *wr);

/* argv[1] -> Content
   argv[2] -> Where to put*/
int main(int argc, char** argv)
{
  startBuffer(buffer);

  /* Missing Error msg */
  FILE *rd = fopen(argv[1], "r");
  FILE *wr = fopen(argv[2], "w");

  startFile(wr);

  int n = atoi(fgets(buffer, bufferSize, rd));

  for( int i = 0; i<n; i++  )
    {
      readProp(rd, wr);
      readSol(rd, wr);
    }

  endFile(wr);

  fclose(rd); fclose(wr);

  return 0;
}

char* startBuffer(char *str)
{
  buffer = (char *)calloc(bufferSize, sizeof(char));

  /* Missing Error msg */

  return buffer;
}

char* readSomething(FILE *rd, char *something)
{
  int i;

  for( i=0; i<bufferSize && !strstr(buffer, something); i++ )
    {
      buffer[i] = fgetc(rd);
      buffer[i<bufferSize ? i+1: i] = '\0';
    }

  return i==bufferSize ? NULL: buffer;
}

void readEmpty(FILE *rd)
{
  char ch;

  for ( ch = '\n'; isspace(ch) && ch != ' '; ch = fgetc(rd) );

  ungetc(ch, rd);
}

char* readLine(FILE *rd)
{
  int i = -1;

  do
    {
      i++;

      buffer[i] = fgetc(rd);
      buffer[i<bufferSize ? i+1: i] = '\0';

    } while ( i<bufferSize-1 && buffer[i] != '\n' );

  return i < bufferSize ? buffer : NULL;
}


void writeSomething(FILE *wr, char *str, char *not, char *sub)
{
  for ( int i=0; str[i]!='\0'; i++ )
    {
      if ( !not || !strchr(not, str[i]) )
	fputc(str[i], wr);

      else if ( sub )
	fputs(sub, wr);
    }
}

void readPropName(FILE *rd, FILE *wr)
{
  writeSomething(wr, "prop_", NULL, NULL);

  readEmpty(rd);
  writeSomething(wr, readLine(rd), "\n", NULL);

  writeSomething(wr, " = \"", NULL, NULL);
}

void readProp(FILE *rd, FILE *wr)
{
  readSomething(rd, "start prop:\n");

  readEmpty(rd);
  readPropName(rd, wr);

  /* Becarefull! Checks are not being done*/
  for( readLine(rd) ; !strstr(buffer, "end prop:"); readLine(rd) )
    writeSomething(wr, buffer, "\n", "\\n");

  writeSomething(wr, "\" === ", NULL, NULL);
}

void readSol(FILE *rd, FILE *wr)
{
  readSomething(rd, "start sol:\n");

  for( readLine(rd) ; !strstr(buffer, "end sol:"); readLine(rd) )
    writeSomething(wr, buffer, "\n", " ");

  writeSomething(wr, "\n\n\n", NULL, NULL);
}

void startFile(FILE *wr)
{
  writeSomething(wr, "{-# LANGUAGE TemplateHaskell #-}\nmodule ParserTest where\nimport Lexer\nimport Parser\nimport AST\nimport Test.QuickCheck\n\n\n", NULL, NULL);
}

void endFile(FILE *wr)
{
  writeSomething(wr, "return []\nrunParserTests = $(verboseCheckAll\n", NULL, NULL);
}
