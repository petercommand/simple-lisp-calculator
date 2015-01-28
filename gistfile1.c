#include <stdlib.h>
#include <stdio.h>
#include <memory.h>

enum {
  OPERATOR,
  NUMERAL,
  SEPARATOR
};
enum {
  TOP_LEVEL,
  NON_TOP_LEVEL
};
typedef struct cmd {
  int cmd_type;
  union {
    int cmd_num;
    char cmd_oper;
  };
} cmd;

typedef struct app_elem{
  int n;
  cmd oper;
  cmd num1;
  cmd num2;
} app_elem;
typedef struct lisp_list{
  struct cmd* data;
  struct lisp_list* priv;
  struct lisp_list* next;
  int line_start;
  int cursor_start;
  int line_end;
  int cursor_end;
  int n; //layer of list, starting from 1
} lisp_list;

void throw_error(int line, int cursor, const char* msg);


app_elem app_push(app_elem elem, cmd data, int line, int cursor){
  switch(elem.n){
  case 0:
    if(data.cmd_type != OPERATOR){
      throw_error(line, cursor, "Not an operator");
    }
    elem.oper = data;
    break;
  case 1:
    if(data.cmd_type != NUMERAL){
      throw_error(line, cursor, "Not a numeral");
    }
    elem.num1 = data;
    break;
  case 2:
    if(data.cmd_type != NUMERAL){
      throw_error(line, cursor, "Not a numeral");
    }
    elem.num2 = data;
    break;
  default:
    throw_error(line, cursor, "Operator applied to too many arguments");
  }
  elem.n++;
  return elem;
}

void throw_error(int line, int cursor, const char* msg){
  fprintf(stderr, "%s at line %d:%d\n", msg, line, cursor);
  exit(1);
}

cmd* parse_num(int* line, int* cursor, char** input){
  int current_num = atoi(*input); // the first input is a number, promised from caller 
  (*input)++;
  (*cursor)++;
  while(**input >= '0' && **input <= '9'){
    current_num *= 10;
    current_num += **input;
    (*input)++;
    (*cursor)++;
  }
  cmd* result = (cmd *)malloc(sizeof(cmd));
  result->cmd_type = NUMERAL;
  result->cmd_num = current_num;
  return result;
}
lisp_list* lexer(char* input){
  int cursor = 1;
  int line = 1;
  //int terminate = 0;
  int paren_num = 0;
  lisp_list* dummy_start = (lisp_list *)malloc(sizeof(lisp_list));
  lisp_list* list_pointer = NULL;
  memset(dummy_start, 0, sizeof(*dummy_start));
  dummy_start->data = NULL;
  dummy_start->n = 0;
  lisp_list* head = dummy_start;
  while(*input != '\0'){
    if(*input == '('){
      paren_num++;
      input++;
      cursor++;
    }
    else if(*input == ')'){
      if(paren_num <= 0){
	throw_error(line, cursor, "Failed to match parenthesis");
      }
      else{
	paren_num--;
      }
      list_pointer = (lisp_list *)malloc(sizeof(lisp_list));
      memset(list_pointer, 0, sizeof(lisp_list));
      list_pointer->priv = head;
      list_pointer->next = NULL;
      list_pointer->cursor_start = cursor;
      list_pointer->line_start = line;
      cmd* result = (cmd *)malloc(sizeof(cmd));
      result->cmd_type = SEPARATOR;
      list_pointer->data = result;
      list_pointer->line_end = line;
      list_pointer->cursor_end = cursor;
      list_pointer->n = paren_num + 1;
      head->next = list_pointer;
      head = head->next;
      input++;
      cursor++;
    }
    else if(*input == ' ' || *input == '\r'){
      //do nothing
      input++;
      cursor++;
    }
    else if(*input == '\n'){
      cursor = 1;
      line++;
      input++;
    }
    else if(*input >= '0' && *input <= '9'){
      list_pointer = (lisp_list *)malloc(sizeof(lisp_list));
      memset(list_pointer, 0, sizeof(lisp_list));
      list_pointer->priv = head;
      list_pointer->next = NULL;
      list_pointer->cursor_start = cursor;
      list_pointer->line_start = line;
      list_pointer->data = parse_num(&line, &cursor, &input);
      list_pointer->line_end = line;
      list_pointer->cursor_end = cursor - 1;
      list_pointer->n = paren_num;
      head->next = list_pointer;
      head = head->next;
    }
    else if(*input == '+' || *input == '-' || *input == '*' || *input == '/'){
      list_pointer = (lisp_list *)malloc(sizeof(lisp_list));
      memset(list_pointer, 0, sizeof(lisp_list));
      list_pointer->priv = head;
      list_pointer->next = NULL;
      list_pointer->cursor_start = cursor;
      list_pointer->line_start = line;
      cmd* result = (cmd *)malloc(sizeof(cmd));
      result->cmd_type = OPERATOR;
      result->cmd_oper = *input;
      list_pointer->data = result;
      list_pointer->line_end = line;
      list_pointer->cursor_end = cursor;
      list_pointer->n = paren_num;
      head->next = list_pointer;
      head = head->next;
      input++;
      cursor++;
    }
    else{
      throw_error(line, cursor, "Unrecognized input");
    }
  }
  return dummy_start;
}

int app(app_elem elem){
  if(elem.n < 3 && elem.oper.cmd_oper != '-'){
    fprintf(stderr, "Operator applied to too few arguments\n");
    exit(1);
  }
  switch(elem.oper.cmd_oper){
  case '+':
    return elem.num1.cmd_num + elem.num2.cmd_num;
  case '-':
    if(elem.n == 3){
      return elem.num1.cmd_num - elem.num2.cmd_num;
    }
    return (-1) * elem.num1.cmd_num;
  case '*':
    return elem.num1.cmd_num * elem.num2.cmd_num;
  case '/':
    return elem.num1.cmd_num / elem.num2.cmd_num;
  }
}


int eval(lisp_list* list, lisp_list** end, int level){
  if(level == TOP_LEVEL && list->n != 1){
    throw_error(list->line_start, list->cursor_start, "Not a top level operator");
  }
  if(list->data->cmd_type != OPERATOR){
    throw_error(list->line_start, list->cursor_start, "Not an operator");
  }
  for(lisp_list* i = list;i != NULL;i = i->next){
    if(i != NULL && i->next != NULL && i->data->cmd_type == SEPARATOR && i->n == 1){
      throw_error(i->next->line_start, i->next->cursor_start, "Unexpected input");
    }
  }
  app_elem elem;
  memset(&elem, 0, sizeof(elem));
  lisp_list* head = list;
  lisp_list* current_end = NULL;
  lisp_list* tail = list;
  int current_n = head->n;
  cmd temp;
  while(tail != NULL && tail->n >= current_n && !(tail->data->cmd_type == SEPARATOR && tail->n == current_n)){
    tail = tail->next;
  }
  if(level == TOP_LEVEL && (tail != NULL && tail->data->cmd_type != SEPARATOR)){
    throw_error(tail->line_start, tail->cursor_start, "Failed to parse input");
  }
  if(tail->data->cmd_type == SEPARATOR){
    tail = tail->next;
  }
  *end = tail;
  int terminate = 0;
  while(head != NULL && (head != tail || terminate)){
    if(head == tail){
      terminate = 1;
    }
    if(head->data->cmd_type == SEPARATOR){
      head = head->next;
      continue;
    }
    if(head->n > current_n){
      temp.cmd_type = NUMERAL;
      temp.cmd_num = eval(head, &current_end, NON_TOP_LEVEL);
      elem = app_push(elem, temp, head->line_start, head->cursor_start);
      head = current_end;
    }
    else{
      elem = app_push(elem, *head->data, head->line_start, head->cursor_start);
      head = head->next;
    }
    
  }
  return app(elem);
}

int top_level_eval(lisp_list* list){
  lisp_list* temp;
  return eval(list, &temp, TOP_LEVEL);
}

int main(){
  size_t nbytes = 512;
  char* input = (char *)malloc(nbytes);
  memset(input, 0, nbytes);
  getline(&input, &nbytes, stdin);
  if(input[0] == '\0'){
    exit(0);
  }
  lisp_list* list = lexer(input);
  int result = top_level_eval(list->next);
  
  //cleanup
  lisp_list* temp;
  while(list != NULL){
    temp = list->next;
    if(list->data != NULL){
      free(list->data);
    }
    free(list);
    list = temp;
  }
  printf("%d\n", result);
  free(input);
  return 0;
}



