(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[383],{12265:function(B){B.exports={databases:"databases___35H4X","detail-wrap":"detail-wrap___1IA0t","tops-wrap":"tops-wrap___2AQns",tops:"tops___2lHCZ"}},59498:function(B,_,e){"use strict";e.r(_),e.d(_,{default:function(){return re}});var o=e(11849),c=e(3182),P=e(57663),r=e(71577),v=e(34792),U=e(48086),y=e(86582),g=e(2824),C=e(94043),T=e.n(C),i=e(67294),O=e(83539),j=e(13062),N=e(71230),M=e(89032),A=e(15746),S=e(69886),u=e(85893),Z=function(l){var D=(0,i.useCallback)(function(){var f=new S.by("dataActionPie".concat(l.id),{autoFit:!0,height:250,appendPadding:10,data:l.types,angleField:"value",colorField:"type",radius:.75,legend:{position:"bottom"},label:{type:"spider",labelHeight:28,content:`{name}
{percentage}`},interactions:[{type:"element-selected"},{type:"element-active"}]});f.update({theme:{styleSheet:{brandColor:"#025DF4",paletteQualitative10:["#025DF4","#DB6BCF","#2498D1","#BBBDE6","#4045B2","#21A97A","#FF745A","#007E99","#FFA8A8","#2391FF"],paletteQualitative20:["#025DF4","#DB6BCF","#2498D1","#BBBDE6","#4045B2","#21A97A","#FF745A","#007E99","#FFA8A8","#2391FF","#FFC328","#A0DC2C","#946DFF","#626681","#EB4185","#CD8150","#36BCCB","#327039","#803488","#83BC99"]}}}),f.render()},[l.id]);return(0,i.useEffect)(function(){D()},[D]),(0,u.jsx)("div",{id:"dataActionPie"+l.id})},L=Z,F=e(39004),a=function(l){var D=(0,i.useCallback)(function(){var f=new F.kL({container:"dataRecentlyLine".concat(l.id),autoFit:!0,height:200});f.data(l.ranges),f.scale({value:{min:0,nice:!1},year:{range:[0,1]}}),f.tooltip({}),f.axis("value",{label:{}}),f.area().position("year*value"),f.line().position("year*value"),f.render()},[l.id]);return(0,i.useEffect)(function(){D()},[D]),(0,u.jsx)("div",{id:"dataRecentlyLine"+l.id})},n=a,d=function(l){return(0,u.jsx)(N.Z,{className:l.style["detail-wrap"],gutter:20,children:(0,u.jsx)(A.Z,{span:24,children:(0,u.jsxs)(N.Z,{children:[(0,u.jsxs)(A.Z,{span:12,className:"text-c",children:[(0,u.jsx)("h3",{children:"\u64CD\u4F5C\u884C\u4E3A\u5206\u5E03"}),(0,u.jsx)(L,{types:l.types,id:l.id})]}),(0,u.jsx)(A.Z,{span:12,className:"text-c",children:(0,u.jsxs)("div",{className:"text-c mt-15 mb-20",children:[(0,u.jsx)("h3",{children:"\u8FD1\u4E03\u5929\u8BBF\u95EE\u6570\u636E\u7EDF\u8BA1"}),(0,u.jsx)(n,{ranges:l.ranges,id:l.id})]})})]})})})},m=d,t=e(12265),K=e.n(t),ae=e(66572),Y=e(24480),$=e(6650),ne=e(84674),se=function(){var l=(0,i.useState)([]),D=(0,g.Z)(l,2),f=D[0],ue=D[1],ie=(0,i.useState)(""),V=(0,g.Z)(ie,2),z=V[0],H=V[1],le=(0,i.useState)(0),J=(0,g.Z)(le,2),oe=J[0],de=J[1],ce=(0,i.useState)({pageSize:10,pageNo:1}),Q=(0,g.Z)(ce,2),R=Q[0],me=Q[1],pe=(0,i.useState)({types:[],ranges:[]}),X=(0,g.Z)(pe,2),fe=X[0],q=X[1],ve=[{title:"\u6570\u636E\u5E93\u8868",dataIndex:"tableName",key:"tableName",align:"center",render:function(h,s){var b="-";if(s.tableNameDesc)b=s.tableNameDesc;else{var p=s.tableName.split("#");b=p[p.length-1]}return(0,u.jsx)("div",{style:{wordWrap:"break-word",wordBreak:"break-word"},children:b})}},{title:"\u88AB\u8BBF\u95EE\u6B21\u6570",dataIndex:"visitedCount",key:"visitedCount",align:"center",search:!1},{title:"\u5E38\u8BBF\u95EE\u4EBA\u5458",key:"usualVisitedUser",dataIndex:"usualVisitedUser",align:"center",search:!1},{title:"\u6700\u540E\u8BBF\u95EE\u65F6\u95F4",key:"lastVisitedDate",dataIndex:"lastVisitedDate",valueType:"date",hideInSetting:!0,search:!1,align:"center"},{title:"\u64CD\u4F5C",key:"action",align:"center",search:!1,render:function(h,s){return[(0,u.jsx)(r.Z,{type:"link",size:"small",onClick:function(){if(z===s.id){H(""),q({types:[],ranges:[]});return}if(s.visitedCount!==0){var p=Y.Z.getRangeStartAndEndTime(7,"YYYY-mm-dd HH:MM:SS"),I=Y.Z.getRangeTime(7,"YYYY-mm-dd");(0,$.Xd)({msTableName:s.tableName,startTime:p[0],endTime:p[1],tableName:s.tableName}).then(function(ge){var G=ge.data;if(G&&G.length>0){var he=G.map(function(x,W){return{year:I[W],value:x}}),ee=(0,y.Z)(ne.q);(0,$.et)({tableName:s.tableName}).then(function(x){x.data&&x.data.length>0&&ee.forEach(function(W){x.data.forEach(function(ye){var te=JSON.parse(ye),_e=te.dbType,Ce=te.dbTypeTimes;W.type===_e&&(W.value=parseInt(Ce))})})}),q({ranges:he,types:ee}),setTimeout(function(){H(s.id)},400)}})}else U.ZP.info("".concat(s.tableName,"\u8868\u8BBF\u88AB\u95EE\u6B21\u6570\u4E3A0\uFF0C\u6682\u65E0\u8BE6\u60C5\u6570\u636E")).then()},children:"\u8BE6\u60C5"},"action")]}}],w=(0,i.useCallback)(function(E){(0,ae.m)(E).then(function(h){var s=h.data;if(s){var b=JSON.parse(s);de(b.total);var p=JSON.parse(b.rows);ue(p.map(function(I){return I.id=Y.Z.uuid(),I}))}})},[]);return(0,i.useEffect)(function(){w(R)},[w,R]),(0,u.jsx)("div",{className:K().databases,children:(0,u.jsx)(O.ZP,{columns:ve,rowKey:"id",pagination:{showSizeChanger:!1,total:oe,pageSize:R.pageSize,current:R.pageNo,onChange:function(h,s){me({pageNo:h,pageSize:s}),w({pageNo:h,pageSize:s})}},toolBarRender:!1,onSubmit:function(){var E=(0,c.Z)(T().mark(function h(s){return T().wrap(function(p){for(;;)switch(p.prev=p.next){case 0:if(!s.tableName){p.next=3;break}return p.next=3,w((0,o.Z)((0,o.Z)({},R),s));case 3:case"end":return p.stop()}},h)}));return function(h){return E.apply(this,arguments)}}(),expandable:{expandedRowRender:function(h,s){return(0,i.createElement)(m,(0,o.Z)((0,o.Z)({},fe),{},{id:s,key:s,style:K()}))},expandIcon:function(){return!1},expandedRowKeys:[z]},dataSource:f})})},re=se},84674:function(B,_,e){"use strict";e.d(_,{q:function(){return o},Q:function(){return c}});var o=[{type:"insert",value:0},{type:"delete",value:0},{type:"update",value:0},{type:"select",value:0}],c=[{type:"\u4E0A\u5348",value:0},{type:"\u4E0B\u5348",value:0},{type:"\u665A\u4E0A",value:0}]},66572:function(B,_,e){"use strict";e.d(_,{m:function(){return U}});var o=e(11849),c=e(3182),P=e(94043),r=e.n(P),v=e(25377);function U(g,C){return y.apply(this,arguments)}function y(){return y=(0,c.Z)(r().mark(function g(C,T){return r().wrap(function(O){for(;;)switch(O.prev=O.next){case 0:return O.abrupt("return",(0,v.WY)("/api/skyflying/getCoarseCountsOfTableName",(0,o.Z)({method:"GET",params:C},T||{})));case 1:case"end":return O.stop()}},g)})),y.apply(this,arguments)}},6650:function(B,_,e){"use strict";e.d(_,{F$:function(){return U},Xd:function(){return g},mt:function(){return T},et:function(){return N},FP:function(){return A},Bx:function(){return u},wA:function(){return L}});var o=e(11849),c=e(3182),P=e(94043),r=e.n(P),v=e(25377);function U(a){return y.apply(this,arguments)}function y(){return y=(0,c.Z)(r().mark(function a(n){return r().wrap(function(m){for(;;)switch(m.prev=m.next){case 0:return m.abrupt("return",(0,v.WY)("/api/skyflying/getCoarseCountsOfUser",(0,o.Z)({method:"GET"},n||{})));case 1:case"end":return m.stop()}},a)})),y.apply(this,arguments)}function g(a,n){return C.apply(this,arguments)}function C(){return C=(0,c.Z)(r().mark(function a(n,d){return r().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,v.WY)("/api/skyflying/getCountsOfUserRecentSevenDays",(0,o.Z)({method:"GET",params:n},d||{})));case 1:case"end":return t.stop()}},a)})),C.apply(this,arguments)}function T(a,n){return i.apply(this,arguments)}function i(){return i=(0,c.Z)(r().mark(function a(n,d){return r().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,v.WY)("/api/skyflying/getVisitRate",(0,o.Z)({method:"GET",params:{username:n}},d||{})));case 1:case"end":return t.stop()}},a)})),i.apply(this,arguments)}function O(a,n){return j.apply(this,arguments)}function j(){return j=_asyncToGenerator(_regeneratorRuntime.mark(function a(n,d){return _regeneratorRuntime.wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",request("/api/skyflying/getCountsOfAllRecentSevenDays",_objectSpread({method:"GET",params:n},d||{})));case 1:case"end":return t.stop()}},a)})),j.apply(this,arguments)}function N(a,n){return M.apply(this,arguments)}function M(){return M=(0,c.Z)(r().mark(function a(n,d){return r().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,v.WY)("/api/skyflying/getCountsOfUser",(0,o.Z)({method:"GET",params:n},d||{})));case 1:case"end":return t.stop()}},a)})),M.apply(this,arguments)}function A(a,n){return S.apply(this,arguments)}function S(){return S=(0,c.Z)(r().mark(function a(n,d){return r().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,v.WY)("/api/skyflying/getUserOperationTypeCount",(0,o.Z)({method:"GET",params:n},d||{})));case 1:case"end":return t.stop()}},a)})),S.apply(this,arguments)}function u(a,n){return Z.apply(this,arguments)}function Z(){return Z=(0,c.Z)(r().mark(function a(n,d){return r().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,v.WY)("/api/skyflying/getUserUsualAndUnusualData",(0,o.Z)({method:"GET",params:n},d||{})));case 1:case"end":return t.stop()}},a)})),Z.apply(this,arguments)}function L(a,n){return F.apply(this,arguments)}function F(){return F=(0,c.Z)(r().mark(function a(n,d){return r().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,v.WY)("/api/skyflying/getCoarseCountsOfOneUser",(0,o.Z)({method:"GET",params:n||{}},d||{})));case 1:case"end":return t.stop()}},a)})),F.apply(this,arguments)}}}]);
