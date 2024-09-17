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
@Table(name = "product_skin_type", indexes = {
		@Index(name = "idx_product_skin_type_product_id", columnList = "product_id"),
		@Index(name = "idx_product_skin_type_skin_type_id", columnList = "skin_type_id")
})
@Entity
@Getter
@Setter
public class ProductSkinType extends EntityBase{
	private static final long serialVersionUID = 1L;

	@NotEmpty(message = "Id of product can not empty")
	@NotNull(message = "Id of product can not null")
	@Column(name = "product_id", length = 700)
	private String productId;
	
	@NotEmpty(message = "Id of skin type can not empty")
	@NotNull(message = "Id of skin type can not null")
	@Column(name = "skin_type_id", length = 700)
	private String skinTypeId;
}
