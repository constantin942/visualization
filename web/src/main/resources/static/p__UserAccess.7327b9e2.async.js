(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[462],{33599:function(P){P.exports={userAccess:"userAccess___1UdBc","detail-wrap":"detail-wrap___2mcEh","tabs-wrap":"tabs-wrap___3PPX8","content-wrap":"content-wrap___2U6yp"}},63994:function(P,A,e){"use strict";e.r(A),e.d(A,{default:function(){return Ce}});var f=e(3182),y=e(57663),x=e(71577),d=e(34792),h=e(48086),a=e(86582),l=e(2824),o=e(94043),s=e.n(o),u=e(67294),B=e(13062),I=e(71230),V=e(89032),w=e(15746),W=e(90387),Q=e(9536),S=e(69886),r=e(85893),M=function(n){return(0,u.useEffect)(function(){var i=new S.by("ActionPie".concat(n.id),{autoFit:!0,height:250,appendPadding:10,data:n.types,angleField:"value",colorField:"type",radius:.75,label:{type:"spider",labelHeight:28,content:`{name}
{percentage}`},interactions:[{type:"element-selected"},{type:"element-active"}]});i.update({theme:{styleSheet:{brandColor:"#025DF4",paletteQualitative10:["#025DF4","#DB6BCF","#2498D1","#BBBDE6","#4045B2","#21A97A","#FF745A","#007E99","#FFA8A8","#2391FF"],paletteQualitative20:["#025DF4","#DB6BCF","#2498D1","#BBBDE6","#4045B2","#21A97A","#FF745A","#007E99","#FFA8A8","#2391FF","#FFC328","#A0DC2C","#946DFF","#626681","#EB4185","#CD8150","#36BCCB","#327039","#803488","#83BC99"]}}}),i.render()},[]),(0,r.jsx)("div",{id:"ActionPie"+n.id})},c=M,v=function(n){return(0,u.useEffect)(function(){var i=new S.by("VisibleTimePie"+n.id,{autoFit:!0,height:250,appendPadding:10,data:n.intervals,angleField:"value",colorField:"type",radius:.75,label:{type:"spider",labelHeight:28,content:`{name}
{percentage}`},interactions:[{type:"element-selected"},{type:"element-active"}]});i.update({theme:{styleSheet:{brandColor:"#FF6B3B",paletteQualitative10:["#FF6B3B","#626681","#FFC100","#9FB40F","#76523B","#DAD5B5","#0E8E89","#E19348","#F383A2","#247FEA"],paletteQualitative20:["#FF6B3B","#626681","#FFC100","#9FB40F","#76523B","#DAD5B5","#0E8E89","#E19348","#F383A2","#247FEA","#2BCB95","#B1ABF4","#1D42C2","#1D9ED1","#D64BC0","#255634","#8C8C47","#8CDAE5","#8E283B","#791DC9"]}}}),i.render()},[]),(0,r.jsx)("div",{id:"VisibleTimePie"+n.id})},p=v,g=e(39004),t=function(n){return(0,u.useEffect)(function(){var i=new g.kL({container:"RecentlyLine".concat(n.id),autoFit:!0,height:200});i.data(n.ranges),i.scale({value:{min:0,nice:!1},year:{range:[0,1]}}),i.tooltip({showCrosshairs:!0,shared:!0}),i.axis("value",{label:{}}),i.area().position("year*value"),i.line().position("year*value"),i.render()},[]),(0,r.jsx)("div",{id:"RecentlyLine"+n.id})},de=t,re=[{label:"\u7ECF\u5E38\u8BBF\u95EE\u7684\u6570\u636E",value:"higher"},{label:"\u4E0D\u5E38\u8BBF\u95EE\u7684\u6570\u636E",value:"lower"}],ve=function(n){var i,D,C,ee=(0,u.useState)(re[0].value),L=(0,l.Z)(ee,2),G=L[0],te=L[1];return(0,r.jsxs)(I.Z,{className:n.style["detail-wrap"],children:[(0,r.jsxs)(w.Z,{span:8,className:n.style["tabs-wrap"],children:[(0,r.jsx)(Q.Z,{block:!0,options:re,value:G,onChange:te}),(0,r.jsx)("div",{className:n.style["content-wrap"],children:(i=n.details)===null||i===void 0||(D=i.higherOrLower)===null||D===void 0||(C=D[G])===null||C===void 0?void 0:C.map(function($){return(0,r.jsxs)("div",{className:"flex-x-sb",children:[(0,r.jsx)("span",{children:$.visitedData}),(0,r.jsxs)("span",{children:[$.visitedCount," \u6B21"]})]},$.visitedData)})})]}),(0,r.jsxs)(w.Z,{span:16,className:"pl-10 pr-20",children:[(0,r.jsxs)(I.Z,{children:[(0,r.jsxs)(w.Z,{span:12,className:"text-c",children:[(0,r.jsx)("h3",{children:"\u64CD\u4F5C\u884C\u4E3A\u5206\u5E03"}),(0,r.jsx)(c,{types:n.details.types,id:n.id})]}),(0,r.jsxs)(w.Z,{span:12,className:"text-c",children:[(0,r.jsx)("h3",{children:"\u8BBF\u95EE\u65F6\u6BB5\u5206\u5E03"}),(0,r.jsx)(p,{intervals:n.details.intervals,id:n.id})]})]}),(0,r.jsxs)("div",{className:"text-c mt-15 mb-20",children:[(0,r.jsx)("h3",{children:"\u8FD1\u4E03\u5929\u8BBF\u95EE\u6570\u636E\u7EDF\u8BA1"}),(0,r.jsx)(de,{ranges:n.details.ranges,id:n.id})]})]})]})},me=ve,pe=e(33599),se=e.n(pe),fe=e(73016),Y=e(6650),he=e(11849),ge=e(32773);function be(E){return X.apply(this,arguments)}function X(){return X=_asyncToGenerator(_regeneratorRuntime.mark(function E(n){return _regeneratorRuntime.wrap(function(D){for(;;)switch(D.prev=D.next){case 0:return D.abrupt("return",request("/api/skyflying/getAllUserNamePortraitByVisitedTime",_objectSpread({method:"GET"},n||{})));case 1:case"end":return D.stop()}},E)})),X.apply(this,arguments)}function ye(E,n){return z.apply(this,arguments)}function z(){return z=(0,f.Z)(s().mark(function E(n,i){return s().wrap(function(C){for(;;)switch(C.prev=C.next){case 0:return C.abrupt("return",(0,ge.WY)("/api/skyflying/getAllUserPortraitByVisitedTime",(0,he.Z)({method:"GET",params:n||{}},i||{})));case 1:case"end":return C.stop()}},E)})),z.apply(this,arguments)}var q=e(24480),ue=e(84674),De=function(){var n=(0,u.useState)(),i=(0,l.Z)(n,2),D=i[0],C=i[1],ee=(0,u.useState)(""),L=(0,l.Z)(ee,2),G=L[0],te=L[1],$=(0,u.useState)({types:[],intervals:[],ranges:[],higherOrLower:{higher:[],lower:[]}}),ie=(0,l.Z)($,2),Ee=ie[0],Ae=ie[1],Be=[{title:"\u7528\u6237\u540D",dataIndex:"userName",key:"applicationUserName",align:"center"},{title:"\u8BBF\u95EE\u6B21\u6570",dataIndex:"visitedCount",key:"visitedCount",search:!1,width:100,align:"center"},{title:"\u5E38\u7528\u6570\u636E",dataIndex:"usualVisitedData",key:"usualVisitedData",align:"center",search:!1,render:function(U,m){var F=JSON.parse(m.usualVisitedData),_=F.tableName,j=F.tableNameDesc,N=_.split(","),k="-";return j?k=j:N.length>1&&(k=N[N.length-1]),(0,r.jsx)("div",{style:{wordWrap:"break-word",wordBreak:"break-word"},children:k})}},{title:"\u6700\u540E\u8BBF\u95EE\u65F6\u95F4",dataIndex:"lastVisitedDate",key:"lastVisitedDate",search:!1,align:"center"},{title:"\u64CD\u4F5C",key:"action",align:"center",search:!1,render:function(U,m){return[(0,r.jsx)(x.Z,{type:"link",size:"small",onClick:function(){if(m.visitedCount!==0){var _=JSON.parse(m.usualVisitedData),j=_.tableName,N=q.Z.getRangeStartAndEndTime(7,"YYYY-mm-dd HH:MM:SS"),k=q.Z.getRangeTime(7,"YYYY-mm-dd HH:MM:SS");(0,Y.Xd)({applicationUserName:m.userName,startTime:N[0],endTime:N[1],tableName:j}).then(function(Ue){var ae=Ue.data;if(ae&&ae.length>0){var Fe=ae.map(function(T,Z){return{year:k[Z],value:T}}),oe=(0,a.Z)(ue.q);(0,Y.FP)({userName:m.userName}).then(function(T){T.data&&T.data.length>0&&oe.forEach(function(Z){T.data.forEach(function(H){var R=JSON.parse(H),b=R.dbType,J=R.dbTypeTimes;Z.type===b&&(Z.value=parseInt(J))})})});var K=(0,a.Z)(ue.Q);ye({userName:m.userName}).then(function(T){var Z=JSON.parse(T.data),H=JSON.parse(Z.rows);if(H&&H.length>0){var R=H[0];K[0].value=R.forenoonCount|0,K[1].value=R.afternoonCount|0,K[2].value=R.nightCount|0}(0,Y.Bx)({applicationUserName:m.userName}).then(function(b){if(b.data){b.data.forEach(function(ne){var ce=ne.visitedData.split("#");return ne.visitedData=ce[ce.length-1],ne});var J=Math.ceil(b.data.length/2),_e=b.data.splice(0,J),Te=b.data.splice(-J);Ae(function(){return{types:oe,intervals:K,ranges:Fe,higherOrLower:{higher:_e,lower:Te}}}),te(m.id)}})})}})}else h.ZP.info("".concat(m.userName,"\u7528\u6237\u8BBF\u95EE\u6B21\u6570\u4E3A0\uFF0C\u6682\u65E0\u8BE6\u60C5\u6570\u636E")).then()},children:"\u8BE6\u60C5"},"action")]}}],le=function(){(0,Y.F$)().then(function(U){var m=U.data;m&&m.length>0?C(m.map(function(F){return F.id=q.Z.uuid(),F})):C([])})};return(0,u.useEffect)(function(){le()},[]),(0,r.jsx)("div",{className:se().userAccess,children:(0,r.jsx)(fe.Z,{columns:Be,rowKey:"id",onSubmit:function(){var O=(0,f.Z)(s().mark(function U(m){return s().wrap(function(_){for(;;)switch(_.prev=_.next){case 0:Object.keys(m).length>0?(0,Y.wA)(m).then(function(j){console.log(j)}):le();case 1:case"end":return _.stop()}},U)}));return function(U){return O.apply(this,arguments)}}(),expandable:{expandedRowRender:function(U,m){return(0,r.jsx)(me,{details:Ee,id:m,style:se()},m)},expandIcon:function(){return!1},expandedRowKeys:[G]},dataSource:D})})},Ce=De},84674:function(P,A,e){"use strict";e.d(A,{q:function(){return f},Q:function(){return y}});var f=[{type:"insert",value:0},{type:"delete",value:0},{type:"update",value:0},{type:"select",value:0}],y=[{type:"\u4E0A\u5348",value:0},{type:"\u4E0B\u5348",value:0},{type:"\u665A\u4E0A",value:0}]},6650:function(P,A,e){"use strict";e.d(A,{F$:function(){return a},Xd:function(){return o},et:function(){return I},FP:function(){return w},Bx:function(){return Q},wA:function(){return r}});var f=e(11849),y=e(3182),x=e(94043),d=e.n(x),h=e(32773);function a(c){return l.apply(this,arguments)}function l(){return l=(0,y.Z)(d().mark(function c(v){return d().wrap(function(g){for(;;)switch(g.prev=g.next){case 0:return g.abrupt("return",(0,h.WY)("/api/skyflying/getCoarseCountsOfUser",(0,f.Z)({method:"GET"},v||{})));case 1:case"end":return g.stop()}},c)})),l.apply(this,arguments)}function o(c,v){return s.apply(this,arguments)}function s(){return s=(0,y.Z)(d().mark(function c(v,p){return d().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,h.WY)("/api/skyflying/getCountsOfUserRecentSevenDays",(0,f.Z)({method:"GET",params:v},p||{})));case 1:case"end":return t.stop()}},c)})),s.apply(this,arguments)}function u(c,v){return B.apply(this,arguments)}function B(){return B=_asyncToGenerator(_regeneratorRuntime.mark(function c(v,p){return _regeneratorRuntime.wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",request("/api/skyflying/getCountsOfAllRecentSevenDays",_objectSpread({method:"GET",params:v},p||{})));case 1:case"end":return t.stop()}},c)})),B.apply(this,arguments)}function I(c,v){return V.apply(this,arguments)}function V(){return V=(0,y.Z)(d().mark(function c(v,p){return d().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,h.WY)("/api/skyflying/getCountsOfUser",(0,f.Z)({method:"GET",params:v},p||{})));case 1:case"end":return t.stop()}},c)})),V.apply(this,arguments)}function w(c,v){return W.apply(this,arguments)}function W(){return W=(0,y.Z)(d().mark(function c(v,p){return d().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,h.WY)("/api/skyflying/getUserOperationTypeCount",(0,f.Z)({method:"GET",params:v},p||{})));case 1:case"end":return t.stop()}},c)})),W.apply(this,arguments)}function Q(c,v){return S.apply(this,arguments)}function S(){return S=(0,y.Z)(d().mark(function c(v,p){return d().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,h.WY)("/api/skyflying/getUserUsualAndUnusualData",(0,f.Z)({method:"GET",params:v},p||{})));case 1:case"end":return t.stop()}},c)})),S.apply(this,arguments)}function r(c,v){return M.apply(this,arguments)}function M(){return M=(0,y.Z)(d().mark(function c(v,p){return d().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,h.WY)("/api/skyflying/getCoarseCountsOfOneUser",(0,f.Z)({method:"GET",params:v||{}},p||{})));case 1:case"end":return t.stop()}},c)})),M.apply(this,arguments)}},24480:function(P,A,e){"use strict";e.d(A,{Z:function(){return x}});var f=e(69610),y=e(54941),x=function(){function d(){(0,f.Z)(this,d)}return(0,y.Z)(d,null,[{key:"timeStampToTime",value:function(a){var l=new Date(parseInt(a));return l.toJSON().substring(0,10).replace("T","")}},{key:"getLocalTime",value:function(a){var l=arguments.length>1&&arguments[1]!==void 0?arguments[1]:"YYYY-mm-dd HH:MM:SS",o,s={"Y+":a.getFullYear().toString(),"m+":(a.getMonth()+1).toString(),"d+":a.getDate().toString(),"H+":a.getHours().toString(),"M+":a.getMinutes().toString(),"S+":a.getSeconds().toString()},u=l;for(var B in s)o=new RegExp("("+B+")").exec(l),o&&(u=u.replace(o[1],o[1].length==1?s[B]:s[B].padStart(o[1].length,"0")));return u}},{key:"uuid",value:function(){for(var a=[],l="0123456789abcdef",o=0;o<36;o++)a[o]=l.substr(Math.floor(Math.random()*16),1);return a[14]="4",a[19]=l.substr(a[19]&3|8,1),a[8]=a[13]=a[18]=a[23]="-",a.join("").replace(new RegExp(/(-)/g),"")}},{key:"getRangeTime",value:function(a,l){for(var o=[],s=a-1;s>=0;s--){var u=void 0;s===a-1?u=new Date(Date.now()-s*24*60*60*1e3).setHours(0,0,1,0):u=new Date(Date.now()-s*24*60*60*1e3).setHours(23,59,59,999),l?o.push(d.getLocalTime(new Date(u),l)):o.push(u)}return o}},{key:"getRangeStartAndEndTime",value:function(a,l){for(var o=[],s=a-1;s>=0;s--){var u=void 0;s===a-1?u=new Date(Date.now()-s*24*60*60*1e3).setHours(0,0,0,0):s===0?u=new Date(Date.now()-(s-1)*24*60*60*1e3).setHours(0,0,0,0):u=new Date(Date.now()-s*24*60*60*1e3).setHours(23,59,59,999),l?o.push(d.getLocalTime(new Date(u),l)):o.push(u)}return[o[0],o[a-1]]}}]),d}();x.arrayDeDuplication=function(d){return Array.from(new Set(d))}}}]);