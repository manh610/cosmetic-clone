package com.cosmetic.gg.entity.attribute;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "product_item", indexes = {
    @Index(name = "idx_product_item_product_id", columnList = "product_id"),
    @Index(name = "idx_product_item_value", columnList = "value")
})
@Entity
@Getter
@Setter
public class ProductItem extends EntityBase{
	
	@Column(name = "value")
	private String value;

	@NotEmpty(message = "Id of product can not empty")
	@NotNull(message = "Id of product can not null")
	@Column(name = "product_id")
	private String productId;
	
	@NotEmpty(message = "Id of value detail can not empty")
	@NotNull(message = "Id of value detail can not null")
	@Column(name = "value_detail_id")
	private String valueDetailId;
}
