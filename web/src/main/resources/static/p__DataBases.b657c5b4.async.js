(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[383],{12265:function(M){M.exports={databases:"databases___35H4X","detail-wrap":"detail-wrap___1IA0t","tops-wrap":"tops-wrap___2AQns",tops:"tops___2lHCZ"}},59498:function(M,T,e){"use strict";e.r(T),e.d(T,{default:function(){return fe}});var c=e(3182),o=e(11849),$=e(57663),l=e(71577),C=e(34792),w=e(48086),E=e(86582),h=e(2824),U=e(94043),Z=e.n(U),d=e(67294),A=e(60355),k=e(84674),J=e(12265),x=e.n(J),R=e(24480),W=e(66572),q=e(13062),P=e(71230),ie=e(89032),F=e(15746),K=e(69610),j=e(54941),a=e(81306),n=e(19809),i=e(69886),u=e(85893),t=i.G2.getEngine("canvas"),le=function(I){(0,a.Z)(p,I);var m=(0,n.Z)(p);function p(g){var r;return(0,K.Z)(this,p),r=m.call(this,g),r.state={chart:null},r}return(0,j.Z)(p,[{key:"componentDidMount",value:function(){var r=this,S=new i.by("dataActionPie".concat(this.props.id),{autoFit:!0,height:250,appendPadding:10,data:this.props.types,angleField:"value",colorField:"type",radius:.75,tooltip:{formatter:function(y){var O=y.value,B=y.type,V=r.props.types.map(function(G){return G.value}).reduce(function(G,Q){return G+Q},0);return{name:B,value:(O/V*100).toFixed(2)+"%"}}},legend:{position:"bottom"},label:{type:"spider",labelHeight:28,formatter:function(y,O){var B=new t.Group({});return B.addShape({type:"circle",attrs:{x:0,y:0,width:40,height:50,r:5,fill:O.color}}),B.addShape({type:"text",attrs:{x:10,y:6,text:"".concat(y.type),fill:O.color}}),B.addShape({type:"text",attrs:{x:0,y:20,text:"".concat(R.Z.bigNumberTransform(y.value),"\u6B21 "),fill:O.color,fontWeight:700}}),B}},interactions:[{type:"element-selected"},{type:"element-active"}]});S.update({theme:{styleSheet:{brandColor:"#025DF4",paletteQualitative10:["#025DF4","#DB6BCF","#2498D1","#BBBDE6","#4045B2","#21A97A","#FF745A","#007E99","#FFA8A8","#2391FF"],paletteQualitative20:["#025DF4","#DB6BCF","#2498D1","#BBBDE6","#4045B2","#21A97A","#FF745A","#007E99","#FFA8A8","#2391FF","#FFC328","#A0DC2C","#946DFF","#626681","#EB4185","#CD8150","#36BCCB","#327039","#803488","#83BC99"]}}}),S.render()}},{key:"render",value:function(){return(0,u.jsx)("div",{id:"dataActionPie"+this.props.id})}}],[{key:"getDerivedStateFromProps",value:function(r,S){var f=S.chart;return r.refreshUid==="open"&&f&&f.changeData(r.types),r}}]),p}(d.Component),oe=e(39004),de=function(I){(0,a.Z)(p,I);var m=(0,n.Z)(p);function p(g){var r;return(0,K.Z)(this,p),r=m.call(this,g),r.state={chart:null},r}return(0,j.Z)(p,[{key:"componentDidMount",value:function(){var r=new oe.kL({container:"RecentlyLine".concat(this.props.id),autoFit:!0,height:200});r.data(this.props.ranges),r.scale({value:{min:0,nice:!1},year:{range:[0,1]}}),r.tooltip({showNil:!1,showCrosshairs:!1,title:function(f,y){return"".concat(f,"\uFF1A").concat(y.value," \u6B21")},itemTpl:`<div class="g2-tooltip">
        <div class="g2-tooltip-title">Language</div>
        <ul class="g2-tooltip-list">
          <li class="g2-tooltip-list-item">
            <span class="g2-tooltip-marker"></span>
            <span class="g2-tooltip-name">a</span>:<span class="g2-tooltip-value">70</span>
          </li>
          <li class="g2-tooltip-list-item">
            <span class="g2-tooltip-marker"></span>
            <span class="g2-tooltip-name">b</span>:<span class="g2-tooltip-value">50</span>
          </li>
        </ul>
      </div>`}),this.setState({chart:r}),r.area().position("year*value"),r.line().position("year*value"),r.render()}},{key:"render",value:function(){return(0,u.jsx)("div",{id:"RecentlyLine"+this.props.id})}}],[{key:"getDerivedStateFromProps",value:function(r,S){var f=S.chart;return r.refreshUid==="open"&&f&&f.changeData(r.ranges),r}}]),p}(d.Component),ce=function(m){return(0,u.jsx)(P.Z,{className:m.style["detail-wrap"],gutter:20,children:(0,u.jsx)(F.Z,{span:24,children:(0,u.jsxs)(P.Z,{children:[(0,u.jsxs)(F.Z,{span:12,className:"text-c",children:[(0,u.jsx)("h3",{children:"\u64CD\u4F5C\u884C\u4E3A\u5206\u5E03"}),(0,u.jsx)(le,{types:m.types,id:m.id,refreshUid:m.refreshUid})]}),(0,u.jsx)(F.Z,{span:12,className:"text-c",children:(0,u.jsxs)("div",{className:"text-c mt-15 mb-20",children:[(0,u.jsx)("h3",{children:"\u8FD1\u4E03\u5929\u8BBF\u95EE\u6570\u636E\u7EDF\u8BA1"}),(0,u.jsx)(de,{ranges:m.ranges,id:m.id,refreshUid:m.refreshUid})]})})]})})})},pe=ce,ee=e(6650),me=function(){var m=(0,d.useState)([]),p=(0,h.Z)(m,2),g=p[0],r=p[1],S=(0,d.useState)(""),f=(0,h.Z)(S,2),y=f[0],O=f[1],B=(0,d.useState)(0),V=(0,h.Z)(B,2),G=V[0],Q=V[1],ve=(0,d.useState)(),te=(0,h.Z)(ve,2),he=te[0],ge=te[1],ye=(0,d.useState)({pageSize:10,pageNo:1}),ae=(0,h.Z)(ye,2),N=ae[0],_e=ae[1],Ce=(0,d.useState)({types:[],ranges:[]}),ne=(0,h.Z)(Ce,2),De=ne[0],re=ne[1],Ee=[{title:"\u6570\u636E\u5E93\u8868",dataIndex:"tableName",key:"tableName",align:"center",render:function(_,s){var b="-";if(s.tableNameDesc)b=s.tableNameDesc;else{var v=s.tableName.split("#");b=v[v.length-1]}return(0,u.jsx)("div",{style:{wordWrap:"break-word",wordBreak:"break-word"},children:b})}},{title:"\u88AB\u8BBF\u95EE\u6B21\u6570",dataIndex:"visitedCount",key:"visitedCount",align:"center",search:!1},{title:"\u5E38\u8BBF\u95EE\u4EBA\u5458",key:"usualVisitedUser",dataIndex:"usualVisitedUser",align:"center",search:!1},{title:"\u6700\u540E\u8BBF\u95EE\u65F6\u95F4",key:"lastVisitedDate",dataIndex:"lastVisitedDate",valueType:"dateTime",hideInSetting:!0,search:!1,align:"center"},{title:"\u64CD\u4F5C",key:"action",align:"center",search:!1,render:function(_,s){return[(0,u.jsx)(l.Z,{type:"link",size:"small",onClick:function(){if(y===s.id){O(""),re({types:[],ranges:[]});return}if(s.visitedCount!==0){var v=R.Z.getRangeStartAndEndTime(7,"YYYY-mm-dd HH:MM:SS"),z=R.Z.getRangeTime(7,"YYYY-mm-dd");(0,ee.Xd)({msTableName:s.tableName,startTime:v[0],endTime:v[1],tableName:s.tableName}).then(function(Se){var X=Se.data;if(X&&X.length>0){var be=X.map(function(Y,H){return{year:z[H],value:Y}}),se=(0,E.Z)(k.q);(0,ee.et)({tableName:s.tableName}).then(function(Y){Y.data&&Y.data.length>0&&se.forEach(function(H){Y.data.forEach(function(Te){var ue=JSON.parse(Te),Ue=ue.dbType,Ze=ue.dbTypeTimes;H.type===Ue&&(H.value=parseInt(Ze))})})}),re({ranges:be,types:se}),setTimeout(function(){O(s.id)},400)}})}else w.ZP.info("".concat(s.tableName,"\u8868\u8BBF\u88AB\u95EE\u6B21\u6570\u4E3A0\uFF0C\u6682\u65E0\u8BE6\u60C5\u6570\u636E")).then()},children:"\u8BE6\u60C5"},"action")]}}],L=(0,d.useCallback)(function(D){(0,W.m)(D).then(function(_){var s=_.data;if(s){var b=JSON.parse(s);if(Q(b.total),b.rows){var v=JSON.parse(b.rows);r(v.map(function(z){return z.id=R.Z.uuid(),z}));return}}r([])})},[]);return(0,d.useEffect)(function(){L(N)},[L,N]),(0,u.jsx)("div",{className:x().databases,children:(0,u.jsx)(A.ZP,{columns:Ee,rowKey:"id",pagination:{showSizeChanger:!1,total:G,pageSize:N.pageSize,current:N.pageNo,onChange:function(_,s){_e({pageNo:_,pageSize:s}),L((0,o.Z)({pageNo:_,pageSize:s},he))}},toolBarRender:!1,onSubmit:function(){var D=(0,c.Z)(Z().mark(function _(s){return Z().wrap(function(v){for(;;)switch(v.prev=v.next){case 0:return ge(s),v.next=3,L((0,o.Z)((0,o.Z)({},N),s));case 3:case"end":return v.stop()}},_)}));return function(_){return D.apply(this,arguments)}}(),onReset:(0,c.Z)(Z().mark(function D(){return Z().wrap(function(s){for(;;)switch(s.prev=s.next){case 0:L((0,o.Z)({},N));case 1:case"end":return s.stop()}},D)})),expandable:{expandedRowRender:function(_,s){return(0,d.createElement)(pe,(0,o.Z)((0,o.Z)({},De),{},{id:s,key:s,style:x(),refreshUid:y===""?"close":"open"}))},expandIcon:function(){return!1},expandedRowKeys:[y]},dataSource:g})})},fe=me},84674:function(M,T,e){"use strict";e.d(T,{q:function(){return c}});var c=[{type:"insert",value:0},{type:"delete",value:0},{type:"update",value:0},{type:"select",value:0}],o=[{type:"\u4E0A\u5348",value:0},{type:"\u4E0B\u5348",value:0},{type:"\u665A\u4E0A",value:0}]},66572:function(M,T,e){"use strict";e.d(T,{m:function(){return w}});var c=e(11849),o=e(3182),$=e(94043),l=e.n($),C=e(25377);function w(h,U){return E.apply(this,arguments)}function E(){return E=(0,o.Z)(l().mark(function h(U,Z){return l().wrap(function(A){for(;;)switch(A.prev=A.next){case 0:return A.abrupt("return",(0,C.WY)("/api/skyflying/getCoarseCountsOfTableName",(0,c.Z)({method:"GET",params:U},Z||{})));case 1:case"end":return A.stop()}},h)})),E.apply(this,arguments)}},6650:function(M,T,e){"use strict";e.d(T,{F$:function(){return w},Xd:function(){return h},mt:function(){return Z},et:function(){return J},FP:function(){return R},Bx:function(){return q},us:function(){return K}});var c=e(11849),o=e(3182),$=e(94043),l=e.n($),C=e(25377);function w(a,n){return E.apply(this,arguments)}function E(){return E=(0,o.Z)(l().mark(function a(n,i){return l().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,C.WY)("/api/skyflying/getCoarseCountsOfUsers",(0,c.Z)({method:"GET",params:n},i||{})));case 1:case"end":return t.stop()}},a)})),E.apply(this,arguments)}function h(a,n){return U.apply(this,arguments)}function U(){return U=(0,o.Z)(l().mark(function a(n,i){return l().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,C.WY)("/api/skyflying/getCountsOfUserRecentSevenDays",(0,c.Z)({method:"GET",params:n},i||{})));case 1:case"end":return t.stop()}},a)})),U.apply(this,arguments)}function Z(a,n){return d.apply(this,arguments)}function d(){return d=(0,o.Z)(l().mark(function a(n,i){return l().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,C.WY)("/api/skyflying/getVisitRate",(0,c.Z)({method:"GET",params:{username:n}},i||{})));case 1:case"end":return t.stop()}},a)})),d.apply(this,arguments)}function A(a,n){return k.apply(this,arguments)}function k(){return k=_asyncToGenerator(_regeneratorRuntime.mark(function a(n,i){return _regeneratorRuntime.wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",request("/api/skyflying/getCountsOfAllRecentSevenDays",_objectSpread({method:"GET",params:n},i||{})));case 1:case"end":return t.stop()}},a)})),k.apply(this,arguments)}function J(a,n){return x.apply(this,arguments)}function x(){return x=(0,o.Z)(l().mark(function a(n,i){return l().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,C.WY)("/api/skyflying/getCountsOfUser",(0,c.Z)({method:"GET",params:n},i||{})));case 1:case"end":return t.stop()}},a)})),x.apply(this,arguments)}function R(a,n){return W.apply(this,arguments)}function W(){return W=(0,o.Z)(l().mark(function a(n,i){return l().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,C.WY)("/api/skyflying/getUserOperationTypeCount",(0,c.Z)({method:"GET",params:n},i||{})));case 1:case"end":return t.stop()}},a)})),W.apply(this,arguments)}function q(a,n){return P.apply(this,arguments)}function P(){return P=(0,o.Z)(l().mark(function a(n,i){return l().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,C.WY)("/api/skyflying/getUserUsualAndUnusualData",(0,c.Z)({method:"GET",params:n},i||{})));case 1:case"end":return t.stop()}},a)})),P.apply(this,arguments)}function ie(a,n){return F.apply(this,arguments)}function F(){return F=_asyncToGenerator(_regeneratorRuntime.mark(function a(n,i){return _regeneratorRuntime.wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",request("/api/skyflying/getCoarseCountsOfOneUser",_objectSpread({method:"GET",params:n||{}},i||{})));case 1:case"end":return t.stop()}},a)})),F.apply(this,arguments)}function K(a,n){return j.apply(this,arguments)}function j(){return j=(0,o.Z)(l().mark(function a(n,i){return l().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,C.WY)("/api/skyflying/getCountsOfEveryonRecentSevenDays",(0,c.Z)({method:"GET",params:n||{}},i||{})));case 1:case"end":return t.stop()}},a)})),j.apply(this,arguments)}}}]);
