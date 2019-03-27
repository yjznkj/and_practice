//哈希查找的主要过程是如何建立以哈希表及如何解决元素位置占用的问题；

/* 建立哈希表：

 首先需要初始化哈希表，并且确定哈希表的长度；

并且根据（数据）%（哈希表长度）计算出数据在哈希表中的位置；

解决元素占位问题：

   如何计算出这个数据在哈希表中的位置，但是这个位置上有元素，则将这个位置++，
   将这个数据放入下一个位置，如果这个数据还有元素，就给这个位置继续++，直到找
   到一个位置，这个位置为0，表示这个位置可以存放数据；

***这种解决冲突的方式称为：线性探测法解决冲突；

*/

#include<stdio.h>

#define LEN 13
#define N 11
//int data[N] = { 10, 9, 8, 7, 5, 4, 6, 3, 2, 1, 95 };   //原始数据；

int data[N] = { 10, 9, 8, 23, 5, 4, 6, 24, 2, 1, 88 };   //原始数据；
int hash[LEN] = { 0 };   //哈希表，初始化为0；


void Create()
{
	for (int i = 0; i<N; i++)  //循环将原始数据保存到哈希表中；
	{
		//将关键字插入到哈希表hash中；
		int j = data[i] % 13;  //计算哈希地址；
		printf("data[i]=%d, 余数=%d \n",data[i],j);
		while (hash[j])  //元素位置已被占用；
			j = (++j) % LEN;  //线性探测法解决冲突；
		hash[j] = data[i];
	}
}


int Haxi_Sou(int key)
{
	int i = key%LEN;  //计算哈希地址；
	while (hash[i] && hash[i] != key)   //判断是否冲突；
		i = (++i) % LEN;   //线性探测法解决冲突；
	if (hash[i] == 0)  //查找到开放单元，表示查找失败；
		return -1;  //返回失败值；
	else
		return i;   //返回对应的元素下标；
}


int main(void)
{
	int key;
	Create();  //调用函数创建哈希表；
	printf("哈希表中各元素的值:");
	for (int i = 0; i<LEN; i++)
		printf("%d ", hash[i]);
	printf("\n");
	printf("输入查找的关键字；");
	scanf("%d", &key);


	int pos = Haxi_Sou(key);  //调用函数在哈希表中查找；
	if (pos>0)
		printf("查找成功，该关键字在数组中的下标为 %d !!!", pos);
	else
		printf("查找失败!!!");
	printf("\n");
	system("pause");
	return 0;
}