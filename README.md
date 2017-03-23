## 该程序用于对给定中文文本进行情感分析

### 依赖：
+ jiebaR
+ parallel(windows下无法并行)
+ openxlsx
+ dplyr
+ ggplot2
+ optparse

### 用法：

#### 将用于分析的excel数据放入``input``文件夹, 在命令行下运行R脚本, 例如输入数据为**17_6.xlsx**, 需要处理的列为**content**, 则命令为

```bash
Rscript emotion_analyse.R -i "17_6.xlsx" -c "content"
```

#### 运行完毕后会在``output``文件夹下看到相应的目录存放excel格式的结果和统计图.

### 注：

目前对并行的设定为在**windows**下不进行并行，在**linux**环境下使用16个线程，此处没有考虑**MacOX**的情况，如使用时出现问题请直接修改43行``get_cores()``函数强制返回所需要的线程数即可。
