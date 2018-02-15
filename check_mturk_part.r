# Check participants

# part=read.csv('part_white_2.csv')
# dat=read.csv('dat_white_edit_2.csv')
part=read.csv('part_black_2.csv')
dat=read.csv('dat_black_edit_2.csv')
dat=dat[-1,]
dat=subset(dat,Finished=="True")
paid=intersect(dat$Mturk.ID,part$WorkerId)

part$Approve[which(is.element(part$WorkerId,paid))]="X"
part$Reject[which(is.na(part$Approve))]="X"
part$Approve[which(is.na(part$Approve))]=""
part$Reject[which(is.na(part$Reject))]=""
             
# write.csv(part,'dat_white_edit_2_app.csv')
write.csv(part,'part_black_edit_2_app.csv')
