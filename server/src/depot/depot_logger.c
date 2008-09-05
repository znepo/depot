#import <stdio.h>
#import <string.h>
#import "depot_logger.h"

const char *CONSOLE_OUT = "STDOUT";

void c_log(char *log, char *msg) {
  FILE *fp;
  if(strcmp(CONSOLE_OUT,log) == 0) {
    fprintf(stderr, msg);
  } else {
    fp = fopen(log, "a");    
    fprintf(fp, msg);
    fclose(fp);
  }
}



