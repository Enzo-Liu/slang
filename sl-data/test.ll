; ModuleID = 'slang.ll'
source_filename = "<string>"

@intFormat = unnamed_addr constant [4 x i8] c"%d\0A\00"
@const-str = unnamed_addr constant [16 x i8] c"in if: test sdf\00"
@strFormat = unnamed_addr constant [4 x i8] c"%s\0A\00"
@const-str1 = unnamed_addr constant [13 x i8] c"in else: sdf\00"
@strFormat.1 = unnamed_addr constant [4 x i8] c"%s\0A\00"
@const-str2 = unnamed_addr constant [16 x i8] c"in if: test sdf\00"
@strFormat.2 = unnamed_addr constant [4 x i8] c"%s\0A\00"
@const-str3 = unnamed_addr constant [13 x i8] c"in else: sdf\00"
@strFormat.3 = unnamed_addr constant [4 x i8] c"%s\0A\00"

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
main-entry:
  %0 = call i32 @b(i32 3000, i32 4002)
  %1 = udiv i32 23, 30
  %2 = mul i32 7, 8
  %3 = mul i32 %2, 9
  %4 = mul i32 %3, 1
  %5 = call i32 @a(i32 10000)
  %6 = call i32 @a(i32 %5)
  %7 = add i32 1123, %0
  %8 = add i32 %7, %1
  %9 = add i32 %8, %4
  %10 = add i32 %9, 1
  %11 = add i32 %10, %6
  %12 = call i32 @putInt32(i32 %11)
  br label %if-entry

if-entry:                                         ; preds = %main-entry
  %13 = icmp ugt i32 1, 0
  br i1 %13, label %if-then, label %if-else

if-then:                                          ; preds = %if-entry
  %14 = getelementptr inbounds [16 x i8], [16 x i8]* @const-str, i32 0, i32 0
  %15 = getelementptr inbounds [4 x i8], [4 x i8]* @strFormat.3, i32 0, i32 0
  %16 = call i32 (i8*, ...) @printf(i8* %15, i8* %14)
  br label %if-exit

if-else:                                          ; preds = %if-entry
  %17 = getelementptr inbounds [13 x i8], [13 x i8]* @const-str1, i32 0, i32 0
  %18 = getelementptr inbounds [4 x i8], [4 x i8]* @strFormat.3, i32 0, i32 0
  %19 = call i32 (i8*, ...) @printf(i8* %18, i8* %17)
  br label %if-exit

if-exit:                                          ; preds = %if-else, %if-then
  %20 = phi i32 [ %16, %if-then ], [ %19, %if-else ]
  %21 = call i32 @putInt32(i32 %20)
  %22 = udiv i32 2, 3
  %23 = call i32 @putInt32(i32 %22)
  br label %if-entry1

if-entry1:                                        ; preds = %if-exit
  %24 = icmp ugt i32 0, 0
  br i1 %24, label %if-then1, label %if-else1

if-then1:                                         ; preds = %if-entry1
  %25 = getelementptr inbounds [16 x i8], [16 x i8]* @const-str2, i32 0, i32 0
  %26 = getelementptr inbounds [4 x i8], [4 x i8]* @strFormat.3, i32 0, i32 0
  %27 = call i32 (i8*, ...) @printf(i8* %26, i8* %25)
  br label %if-exit1

if-else1:                                         ; preds = %if-entry1
  %28 = getelementptr inbounds [13 x i8], [13 x i8]* @const-str3, i32 0, i32 0
  %29 = getelementptr inbounds [4 x i8], [4 x i8]* @strFormat.3, i32 0, i32 0
  %30 = call i32 (i8*, ...) @printf(i8* %29, i8* %28)
  br label %if-exit1

if-exit1:                                         ; preds = %if-else1, %if-then1
  %31 = phi i32 [ %27, %if-then1 ], [ %30, %if-else1 ]
  %32 = call i32 @putInt32(i32 %31)
  %33 = add i32 3, 4
  %34 = call i32 @putInt32(i32 %33)
  ret i32 0
}
