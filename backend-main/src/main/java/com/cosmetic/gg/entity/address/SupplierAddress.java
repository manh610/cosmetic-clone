package com.cosmetic.gg.entity.address;

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
@Table(name = "supplier_address")
@Entity
@Getter
@Setter
public class SupplierAddress extends EntityBase{
	
	private String branch;

	@NotEmpty(message = "Id of supplier can not empty")
	@NotNull(message = "Id of supplier can not null")
	@Column(name = "supplier_id", length = 700)
	private String supplierId;
	
	@NotEmpty(message = "Id of address can not empty")
	@NotNull(message = "Id of address can not null")
	@Column(name = "address_id", length = 700)
	private String addressId;
}
