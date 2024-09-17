package com.cosmetic.gg.entity.discount;

import javax.persistence.Column;
import javax.persistence.Entity;
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
@Table(name = "product_discount")
@Entity
@Getter
@Setter
public class ProductDiscount extends EntityBase{

	@NotEmpty(message = "Id of discount can not empty")
	@NotNull(message = "Id of discount can not null")
	@Column(name = "discount_id")
	private String discountId;
	
	@NotEmpty(message = "Id of product not empty")
	@NotNull(message = "Id of product can not null")
	@Column(name = "product_id")
	private String productId;
	
	@Column(name = "quantity")
	private Integer quantity;
}
