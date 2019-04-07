; ModuleID = 'slang.ll'
source_filename = "<string>"

@intFormat = unnamed_addr constant [4 x i8] c"%d\0A\00"
@const-str = unnamed_addr constant [13 x i8] c"in else: sdf\00"
@strFormat = unnamed_addr constant [4 x i8] c"%s\0A\00"
@const-str1 = unnamed_addr constant [16 x i8] c"in if: test sdf\00"
@strFormat.1 = unnamed_addr constant [4 x i8] c"%s\0A\00"
@const-str2 = unnamed_addr constant [13 x i8] c"in else: sdf\00"
@strFormat.2 = unnamed_addr constant [4 x i8] c"%s\0A\00"

declare i32 @printf(i8*, ...)

define i32 @putInt32(i32 %a) {
  %1 = getelementptr inbounds [4 x i8], [4 x i8]* @intFormat, i32 0, i32 0
  %2 = call i32 (i8*, ...) @printf(i8* %1, i32 %a)
  ret i32 %2
}

define i32 @a(i32 %b) {
  %1 = add i32 %b, 1
  ret i32 %1
}

define i32 @b(i32 %c, i32 %d) {
  %1 = sub i32 %c, %d
  %2 = udiv i32 %c, %d
  %3 = udiv i32 %2, %1
  ret i32 %3
}

define i32 @main() {
if-entry:
  %0 = icmp ugt i32 1, 0
  br i1 %0, label %if-then, label %if-else

if-then:                                          ; preds = %if-entry
  %1 = add i32 1, 2
  br label %if-exit

if-else:                                          ; preds = %if-entry
  %2 = getelementptr inbounds [13 x i8], [13 x i8]* @const-str, i32 0, i32 0
  %3 = getelementptr inbounds [4 x i8], [4 x i8]* @strFormat.2, i32 0, i32 0
  %4 = call i32 (i8*, ...) @printf(i8* %3, i8* %2)
  br label %if-exit

if-exit:                                          ; preds = %if-else, %if-then
  %5 = phi i32 [ %1, %if-then ], [ %4, %if-else ]
  %6 = call i32 @putInt32(i32 %5)
  ret void

if-entry1:                                        ; No predecessors!
  %7 = icmp ugt i32 1, 0
  br i1 %7, label %if-then1, label %if-else1

if-then1:                                         ; preds = %if-entry1
  %8 = getelementptr inbounds [16 x i8], [16 x i8]* @const-str1, i32 0, i32 0
  %9 = getelementptr inbounds [4 x i8], [4 x i8]* @strFormat.2, i32 0, i32 0
  %10 = call i32 (i8*, ...) @printf(i8* %9, i8* %8)
  br label %if-exit1

if-else1:                                         ; preds = %if-entry1
  %11 = getelementptr inbounds [13 x i8], [13 x i8]* @const-str2, i32 0, i32 0
  %12 = getelementptr inbounds [4 x i8], [4 x i8]* @strFormat.2, i32 0, i32 0
  %13 = call i32 (i8*, ...) @printf(i8* %12, i8* %11)
  br label %if-exit1

if-exit1:                                         ; preds = %if-else1, %if-then1
  %14 = phi i32 [ %10, %if-then1 ], [ %13, %if-else1 ]
  %15 = call i32 @putInt32(i32 %14)
  %16 = call i32 @b(i32 3000, i32 4002)
  %17 = udiv i32 23, 30
  %18 = mul i32 7, 8
  %19 = mul i32 %18, 9
  %20 = mul i32 %19, 1
  %21 = call i32 @a(i32 10000)
  %22 = call i32 @a(i32 %21)
  %23 = add i32 1123, %16
  %24 = add i32 %23, %17
  %25 = add i32 %24, %20
  %26 = add i32 %25, 1
  %27 = add i32 %26, %22
  %28 = call i32 @putInt32(i32 %27)
  %29 = udiv i32 2, 3
  %30 = call i32 @putInt32(i32 %29)
  %31 = add i32 3, 4
  %32 = call i32 @putInt32(i32 %31)
  ret i32 0
}
