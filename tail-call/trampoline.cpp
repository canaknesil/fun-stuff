#include <functional>

#include <iostream>

using namespace std;

struct bounce;

union bounce_union
{
  function<struct bounce ()> *func;
  int result;
};

struct bounce
{
  bool isFinal;
  union bounce_union val;
};

struct bounce sum_iteration(int *list, int size, int currSum)
{
  if (size == 0) {
    struct bounce b;
    b.isFinal = true;
    b.val.result = currSum;
    return b;
  } else {
    function<struct bounce ()> *func = new function<struct bounce ()>;
    *func = 
      [list, size, currSum](){
	return sum_iteration(list + 1, size - 1, currSum + list[0]);
      };
    struct bounce b;
    b.isFinal = false;
    b.val.func = func;
    return b;
  }
}

int trampoline(struct bounce (*iteration)(int *list, int size, int currSum),
	       int *list, int size, int currSum)
{
  struct bounce b = iteration(list, size, currSum);
  while (!b.isFinal) {
    b = (*(b.val.func))();
  }
  return b.val.result;
}

int main()
{
  int size = 1000000;
  int *list = new int[size];
  for (int i=0; i<size; i++) list[i] = 1;
  
  cout << trampoline(&sum_iteration, list, size, 0) << endl;

  return 0;
}
