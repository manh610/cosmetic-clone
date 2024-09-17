import { Component, EventEmitter, Input, OnInit, Output } from "@angular/core";
import { FlatTreeControl } from "@angular/cdk/tree";
import { MatTreeFlatDataSource, MatTreeFlattener } from "@angular/material/tree";
import { nodeTree, treeDefault } from 'src/app/core/models/common/treeview';

@Component({
  selector: 'app-treeview',
  templateUrl: './treeview.component.html',
  styleUrls: ['./treeview.component.scss']
})
export class TreeviewComponent implements OnInit {
  @Input() nodeData:any;
  @Input() hasPanel = false;
  @Output() _clickedHandler = new EventEmitter();
  @Output() _clickedPanelHandler = new EventEmitter();
  @Output() clickExpandHandler = new EventEmitter();
  activeNode:any;
  _transformer = (node: nodeTree, level: number) => {
    const result:any = {
      expandable: !!node.children && node.children.length > 0,
      name: node.name,
      level: level,
      id: node.id,
      hasChild: node.hasChild
    };
    if(node.children){
      result.children = node.children;
    }
    return result;
  };
  treeControl = new FlatTreeControl<treeDefault>(
    (node) => node.level,
    (node) => node.expandable
  );

  treeFlattener = new MatTreeFlattener(
    this._transformer,
    (node) => node.level,
    (node) => node.expandable,
    (node) => node.children,
  );

  dataSource = new MatTreeFlatDataSource(
    this.treeControl, this.treeFlattener);

  constructor() {
  }
  public renderNodeChanges(data:any) {
    this.dataSource.data = data;
  }
  hasChild = (_: number,
    node: treeDefault) => node.expandable;

  ngOnInit(): void {
    this.dataSource.data = this.nodeData;
  }

  nodeClickedHandler(data:any){
    this._clickedHandler.emit(data);
  }
  nodeExpandHandler(isExpand:any,data: any) {
    const dataNodes = this.treeControl.dataNodes;
    let dataResp = { isExpand: isExpand, data: data, dataNodes: dataNodes }
    this.clickExpandHandler.emit(dataResp);
  }
  expandNode(node:any) {
    let index = this.treeControl.dataNodes.findIndex((x: any) => x.id === node.id);
    this.treeControl.expand(this.treeControl.dataNodes[index]);
  }
  panelHandler(data: any, type: any){
    let dataResp = { data: data, type: type }
    this._clickedPanelHandler.emit(dataResp);
  }
}
